{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module SafeSync.ObjectStore.Webhooks where

-- | SafeSync.ObjectStore.Webhook
-- Webhooks are used for managing object events and are recieved from the S3 object store
-- when an S3 object or bucket is created, updated, or deleted.
-- These are needed to ensure each partition of a file is uploaded to move from the staging bucket
-- to the partitions bucket and then to update the database.


import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Logger (
        runStdoutLoggingT,
        runStderrLoggingT,
        NoLoggingT,
        LoggingT
    )

import Data.Aeson (object, (.=))
import Data.Text.Encoding (decodeUtf8)
import Web.Scotty (
        get, post, delete,
        param, header,
        json, status, request,
        redirect,
        middleware, notFound,
        ScottyM, ActionM 
    )

import Network.Minio

import Network.HTTP.Types (Status (..))
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Database.Persist.Postgresql ( 
        withPostgresqlConn, withPostgresqlPool,
        selectList, (==.), SelectOpt(LimitTo),
        ConnectionString
    )

import Database.Esqueleto.Experimental
    ( Entity(entityVal),
      runSqlConn,
      runSqlPool,
      SqlBackend,
      Value(unValue), selectOne, table, from, where_, (:&) ((:&)), select, leftJoin, on )

import SafeSync.Database.Models
-- import SafeSync.Auth
import SafeSync.Database.Types as DT (UUID)
import SafeSync.Database.Queries
import SafeSync.ObjectStore
import Database.Esqueleto.Experimental (update, val, (^.))
import Data.Maybe (isNothing)
import SafeSync.ObjectStore.Events (unstageObjectPartitions)

handleNotification (Notification nQConfs nTopicConfs _) =
    ()

handleEvent = \case
    ObjectCreated -> ()
    ObjectCreatedPut -> ()
    ObjectCreatedPost  -> ()
    ObjectCreatedCopy  -> ()
    ObjectCreatedMultipartUpload  -> ()
    ObjectRemoved  -> ()
    ObjectRemovedDelete  -> ()
    ObjectRemovedDeleteMarkerCreated  -> ()
    ReducedRedundancyLostObject -> ()

webhooks :: ConnectionString -> ConnectInfo -> ScottyM ()
webhooks dbConnStr minioConn = do
    let minio :: Minio a -> IO (Either MinioErr a) 
        minio = runMinio minioConn

        runDb :: ReaderT SqlBackend (LoggingT IO) a -> IO a
        runDb = runStderrLoggingT . withPostgresqlConn dbConnStr . runSqlConn

        runDbPool :: Word -> ReaderT SqlBackend IO a -> IO a
        runDbPool poolSize action = do
            runStderrLoggingT $
               withPostgresqlPool dbConnStr (fromIntegral poolSize)
                    (liftIO . runSqlPool action)

 
    middleware logStdout

    post "/uploaded/:partitionId" $ do
        let rdb = runDbPool 3
        partId <- param "partitionId" :: ActionM UUID
        now <- liftIO getCurrentTime
        objectId <- liftIO $ rdb $ selectPartitionObjectId partId
        case fmap unValue objectId of
            Nothing -> return ()
            Just objId -> do
                liftIO $ rdb $ updatePartitionUpdatedAt (partId, now)
                parts <- liftIO $ rdb $ selectObjectPartitions objId
                let partitions = map entityVal parts 
                case filter isNothing $ map partitionUpdatedAt partitions of
                    [] -> do
                        result <- liftIO $
                            minio $ unstageObjectPartitions
                                  $ map partitionPartitionId partitions
                        case result of
                            Left err -> handleMinioErr err
                            Right _ -> return ()
                    _ -> return ()

    post "/deleted/:partitionId" $ do
        objectId <- param "partitionId"
        record <- liftIO $ runDb $ selectObject objectId

        case record of
            Nothing -> status $ Status 404 "object not found"
            Just res -> do
                let obj = entityVal res
                partIds <-
                    case objectHeadId obj of
                        Nothing -> return Nothing
                        Just _ -> do
                            record' <- liftIO $ runDb $ selectObjectPartitionIds $ objectObjectId obj
                            return $ Just $ map unValue record'
                json $ object [
                        "objectId" .= objectId,
                        "salt" .= objectSalt obj,
                        "path" .= objectPath obj,
                        "modTime" .= objectModTime obj,
                        "partitionIds" .= partIds
                    ]

    notFound $ do
        status (Status 404 "not found")