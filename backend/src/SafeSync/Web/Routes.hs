-- 2023/01/13
-- Web/Routes.hs

module SafeSync.Web.Routes where

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)

import Web.Scotty

import SafeSync.Database.Models
import SafeSync.Database.Queries
import SafeSync.Database.Connection

import SafeSync.ObjectStore.S3
import SafeSync.ObjectStore.Object

type App = ConnectInfo -> DBInfo -> ScottyM ()

metadataEndpoint :: App 
metadataEndpoint s3Conn dbInfo = do
    let database = runDb dbInfo
    get "/metadata" $ do
        ownerId <- param "ownerId"
        fileIds <- jsonData
        metadata <- liftIO $ database $ getMetadata ownerId fileIds
        json $ toJSON metadata

    get "/metadata/all" $ do
        ownerId <- param "ownerId"
        page <- param "page"
        limit <- param "limit"
        metadata <- liftIO $ database $ getAllMetadata ownerId page limit
        json $ toJSON metadata

    post "/metadata" $ do
        ownerId <- param "ownerId"
        metadata <- jsonData
        newMetadataId <- liftIO $ database $ insertMetadata metadata
        json $ object ["metadataId" .= newMetadataId]

    delete "/metadata" $ do
        fileIds <- jsonData
        liftIO $ database $ deleteMetadata fileIds
        json $ object ["message" .= ("Successfully deleted metadata" :: String)]

partitionsEndpoint :: App
partitionsEndpoint s3Conn dbInfo = do
    let database = runDb dbInfo
    get "/partitions/:partitionId" $ do
        partitionId <- param "partitionId"
        partition <- liftIO $ database $ getPartition partitionId
        case partition of
            Just partitionEntity -> json $ toJSON partitionEntity
            Nothing -> status 404

    post "/partitions" $ do
        partition <- jsonData
        newPartitionId <- liftIO $ database $ insertPartition partition
        json $ object ["partitionId" .= newPartitionId]

    delete "/partitions" $ do
        partitionIds <- jsonData
        liftIO $ database $ deletePartition partitionIds
        json $ object ["message" .= ("Successfully deleted partitions" :: String)]

userEndpoint :: App
userEndpoint s3Conn dbInfo = do
    let database = runDb dbInfo
    get "/users/:userId" $ do
        userId <- param "userId"
        user <- liftIO $ database $ getUser userId
        case user of
            Just userEntity -> json $ toJSON userEntity
            Nothing -> status 404

    post "/users" $ do
        userId <- param "userId"
        liftIO $ database $ updateSyncTime userId
        json $ object ["message" .= ("Successfully updated sync time" :: String)]