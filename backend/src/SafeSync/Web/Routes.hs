-- 2023/01/13
-- Web/Routes.hs

module SafeSync.Web.Routes where

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)

import Web.Scotty

import SafeSync.Database.Models
import SafeSync.Database.Queries
import SafeSync.Database.Connection (runDb)

metadataEndpoint :: ScottyM ()
metadataEndpoint = do
    get "/metadata" $ do
        ownerId <- param "ownerId"
        fileIds <- jsonData
        metadata <- liftIO $ runDb $ getMetadata ownerId fileIds
        json $ toJSON metadata

    get "/metadata/all" $ do
        ownerId <- param "ownerId"
        page <- param "page"
        limit <- param "limit"
        metadata <- liftIO $ runDb $ getAllMetadata ownerId page limit
        json $ toJSON metadata

    post "/metadata" $ do
        ownerId <- param "ownerId"
        metadata <- jsonData
        newMetadataId <- liftIO $ runDb $ insertMetadata metadata
        json $ object ["metadataId" .= newMetadataId]

    delete "/metadata" $ do
        fileIds <- jsonData
        liftIO $ runDb $ deleteMetadata fileIds
        json $ object ["message" .= ("Successfully deleted metadata" :: String)]

partitionsEndpoint :: ScottyM ()
partitionsEndpoint = do
    get "/partitions/:partitionId" $ do
        partitionId <- param "partitionId"
        partition <- liftIO $ runDb $ getPartition partitionId
        case partition of
            Just partitionEntity -> json $ toJSON partitionEntity
            Nothing -> status 404

    post "/partitions" $ do
        partition <- jsonData
        newPartitionId <- liftIO $ runDb $ insertPartition partition
        json $ object ["partitionId" .= newPartitionId]

    delete "/partitions" $ do
        partitionIds <- jsonData
        liftIO $ runDb $ deletePartition partitionIds
        json $ object ["message" .= ("Successfully deleted partitions" :: String)]

userEndpoint :: ScottyM ()
userEndpoint = do
    get "/users/:userId" $ do
        userId <- param "userId"
        user <- liftIO $ runDb $ getUser userId
        case user of
            Just userEntity -> json $ toJSON userEntity
            Nothing -> status 404

    post "/users" $ do
        userId <- param "userId"
        liftIO $ runDb $ updateSyncTime userId
        json $ object ["message" .= ("Successfully updated sync time" :: String)]