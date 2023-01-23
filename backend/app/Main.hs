-- app/Main.hs

import SafeSync.Config
import SafeSync.Server

main :: IO ()
main = do
    config <- fromArgv 
    case config of
        Nothing -> error "Invalid or missing config"
        Just config ->  runServer config