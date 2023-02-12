{-# LANGUAGE OverloadedStrings #-}
-- Main.hs

import System.Environment (getArgs)
import Data.Char (toLower)
import Data.String (fromString)
import Data.Maybe (fromMaybe)

import Data.Aeson (eitherDecode, FromJSON)
import Network.Minio (ConnectInfo, Credentials (..), setCreds, setRegion)

import Web.Scotty

import SafeSync.Routes

import Config

app :: DBInfo -> ConnectInfo -> Port -> IO ()
app (DBInfo dbInfo) minioConn port =
    scotty (fromIntegral port) $ do
        routes dbInfo minioConn

main :: IO ()
main = do
    s3Conf <- s3ConfigFromFile ".config/s3.json"
    dbInfo <- dbInfoFromFile ".config/db.conf"
    serverConf <- serverConfigFromFile ".config/server.json"

    let port = serverPort serverConf
        s3Info = mkS3ConnectInfo s3Conf

    putStrLn $ "Running app safesync at http://localhost:" ++ show port
    print dbInfo
    print s3Info
    app dbInfo s3Info port

mkS3ConnectInfo :: S3Config -> ConnectInfo
mkS3ConnectInfo s3Conf =
    let connInfo :: ConnectInfo
        connInfo = fromString $ s3Host s3Conf ++ ":" ++ show (s3Port s3Conf)

        creds :: Credentials
        creds = Credentials (fromString $ fromMaybe "" $ s3AccessKey s3Conf)
                            (fromString $ fromMaybe "" $ s3SecretKey s3Conf)
        region = fromString (fromMaybe "" $ s3Region s3Conf)
    in
      setRegion region $ setCreds creds connInfo

dbInfoFromFile :: FilePath -> IO DBInfo
dbInfoFromFile path = do
    contents <- readFile path >>= return . fromString
    return $ DBInfo contents

fromJSONFile :: FromJSON a => FilePath -> IO a
fromJSONFile path = do
    contents <- readFile path >>= return . fromString
    case eitherDecode contents of
        Left err -> error err
        Right config -> return config

s3ConfigFromFile :: FilePath -> IO S3Config
s3ConfigFromFile = fromJSONFile

serverConfigFromFile :: FilePath -> IO ServerConfig
serverConfigFromFile = fromJSONFile

-- main = do
--     argv <- getArgs
--     Config serverConf (DBInfo dbString) s3Conf <-
--         case map (map toLower) argv of
--             (fp@"config.json":_) -> configFromFile fp
--             _fileNotFound -> error "Configuration file not found."
--
--     let dbConnInfo :: ConnectionString
--         dbConnInfo = fromString dbString
--         s3Conninfo = mkS3ConnectInfo s3Conf
--
--     safesync (fromIntegral $ serverPort serverConf)
