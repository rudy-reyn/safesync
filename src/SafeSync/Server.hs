-- Server.hs
module SafeSync.Server where

import Data.Time.Clock (getCurrentTime)
import Data.Aeson
import Web.Scotty
import qualified SafeSync.Server.Backend as Backend

runServer = do
    serverStartTime <- getCurrentTime
    putStr "Starting server at "
    print serverStartTime
    putStrLn ""
    scotty 8080 $ do
        get "/download" do
            json Backend.download user_id partitions
