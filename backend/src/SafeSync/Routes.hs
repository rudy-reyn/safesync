{-# LANGUAGE OverloadedStrings #-}
-- 01/28/23
-- src/SafeSync/Routes.hs

module SafeSync.Routes where

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
import qualified Data.Text.Lazy.Encoding as TL
import Web.Scotty (
        get, post, delete,
        param, header,
        json, status, request,
        redirect,
        middleware, notFound,
        ScottyM, ActionM 
    )

import Network.Minio (
        runMinio,
        Minio, MinioErr, ConnectInfo
    )

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
      Value(unValue) )

import SafeSync.Database.Models
-- import SafeSync.Auth
import SafeSync.Database.Types as DT
import SafeSync.Database.Queries
import SafeSync.ObjectStore

routes :: ConnectionString -> ConnectInfo -> ScottyM ()
routes dbConnStr minioConn = do
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

    get "/partition/:partitionId" $ authenticated $ do
        partId <- param "partitionId"
        record <- liftIO $ runDb $ selectPartition partId
        case record of
            Just res -> do
                auth <- header "authorization"
                urlE <- liftIO $ minio $ signedDownloadUrl Partitions partId "" --
                case urlE of
                    Left err -> handleMinioErr err
                    Right url ->
                        json $ object [
                            "partition" .= entityVal res,
                            "url" .= decodeUtf8 url
                        ]
            Nothing -> status $ Status 404 "partition not found"

    get "/object/partitions/:objectId" $ authenticated $ do
        objectId <- param "objectId"
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

    post "/object" $ authenticated $ do
        size <- param "size" :: ActionM Word
        typ' <- param "type" :: ActionM ObjectType
        key <- param "key" :: ActionM DT.Key
        salt <- param "salt" :: ActionM Salt
        json $ object []

    get "/object/:objectId" $ authenticated $ do
        objectId <- param "objectId"
        record <- liftIO $ runDb $ selectList [ObjectObjectId ==. objectId] [LimitTo 1]
        json $ map entityVal record

    get "/u/objects" $ redirect "/user/objects"
    get "/user/objects" $ authenticated $ do
        userId <- param "userId"
        liftIO $ print userId
        record <- liftIO $ runDb $ selectUserObjectIds userId
        json $ map unValue record

    notFound $ do
        status (Status 404 "not found")

    where authenticated :: ActionM () -> ActionM () 
        -- TODO: authoriation logic
          authenticated action = do
            auth <- header "authorization"
            case auth of
                Nothing -> status (Status 401 "Missing credentials")
                Just token -> do
                    action

    -- Web.Scotty.get "/auth" $ do
    --     userId <- param "id" :: ActionM Int64
    --     result <- liftIO $ runDb $ selectList [UserId ==. toSqlKey userId] []
    --     json $ map entityVal result