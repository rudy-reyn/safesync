-- app/Main.hs

import Data.Maybe (fromMaybe)
import qualified Network.Minio as Minio
import qualified Web.Scotty as Scotty
import qualified Network.Wai.Handler.Warp as WHW

import SafeSync.Config
import SafeSync.Server
import SafeSync.Connection

instance FromConfig Minio.ConnectInfo where
    fromConfig conf =
        let config = fromConfig conf :: S3Config
            connInfo :: ConnectInfo
            connInfo =
                case s3IsSecure config of
                    Just True -> "https://" ++ s3Host config ++ ":" ++ show (s3Port config)
                    _ -> "http://" ++ s3Host config ++ ":" ++ show (s3Port config)
            creds = Credentials (fromMaybe "" $ s3AccessKey config) (fromMaybe "" $ s3SecretKey config)
            region = fromMaybe "" (s3Region config)
        in
        setCreds creds $
            setRegion region connInfo 

instance FromConfig DBInfo where
    fromConfig conf = DBInfo $
      let config = dbConfig conf
      in "host="     ++ dbHost config ++
        " port="     ++ show (dbPort config) ++
        " user="     ++ dbUser config ++
        " password=" ++ dbPass config ++
        " dbname="   ++ dbName config

instance FromConfig  WHW.Settings where
    fromConfig conf =
        let config = serverConfig conf in
            settings =
                setHost
                setPort (fromIntegral $ serverPort port)
                $ WHW.defaultSettings
        Scotty.Options (

instance FromConfig  Scotty.Options where
    fromConfig conf =
        let config = serverConfig conf in
        Scotty.Options (


configFromFile :: FilePath -> IO (Maybe Config)
configFromFile filePath = do
    configData <- readFile filePath
    return $ decode configData

fromArgv :: IO (Maybe Config)
fromArgv = do
    args <- getArgs
    return $
       case args of
        [] -> Nothing
        (filePath:_) -> loadConfig filePath
 
main :: IO ()
main = do
    config <- fromArgv 
    case config of
        Nothing -> error "Invalid or missing config"
        Just config ->  runServer config