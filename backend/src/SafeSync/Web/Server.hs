-- Web/Server.hs

module SafeSync.Web.Server where

import Web.Scotty

import SafeSync.Config
import SafeSync.Web.Routes
import SafeSync.Database.Connection
import SafeSync.ObjectStore.Object

runServer :: Options -> ConnectInfo -> DBInfo -> IO ()
runServer serverOptions s3Conn dbInfo config =
    scottyOpts serverOptions $ do
        metadataEndpoint s3Conn dbInfo 
        partitionsEndpoint s3Conn dbInfo
        userEndpoint s3Conn dbInfo