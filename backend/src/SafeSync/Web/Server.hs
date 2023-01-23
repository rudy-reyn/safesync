-- Web/Server.hs

module SafeSync.Web.Server where

import Web.Scotty

import SafeSync.Config
import SafeSync.Web.Routes
import SafeSync.Database.Connection
import SafeSync.ObjectStore.Object

runServer :: Config -> IO ()
runServer config = do
    let database = fromConfig config :: ConnectionString
        objectStore = fromConfig config :: ConnectInfo
    scotty (serverPort config) $ do
        metadataEndpoint database objectStore
        partitionsEndpoint database objectStore
        userEndpoint database objectStore