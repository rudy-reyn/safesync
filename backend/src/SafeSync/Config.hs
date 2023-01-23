-- 2023/01/13
-- Config.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SafeSync.Config where

import GHC.Generics
import Data.ByteString.Lazy (readFile)
import System.Environment (getArgs)
import Data.Word (Word16)

import Data.Aeson

data Config = Config {
    serverHost :: Word16,
    serverPort :: Int,
    dbConfig :: DBConfig,
    s3Config :: S3Config
} deriving (Show, Generic)

instance FromJSON Config

class IsConfig a where
    fromConfig :: Config -> a

data DBConfig = DBConfig {
    dbHost :: String,
    dbPort :: Word16,
    dbUser :: String,
    dbPass :: String,
    dbName :: String
} deriving (Show, Generic)

instance FromJSON DBConfig

data S3Config = S3Config {
    s3Host :: String,
    s3Port :: Word16,
    s3AccessKey :: String,
    s3SecretKey :: String,
    s3Region :: Maybe String
} deriving (Show, Generic)

instance FromJSON S3Config

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig filePath = do
    configData <- readFile filePath
    return $ decode configData

fromArgv :: IO (Maybe Config)
fromArgv = do
    args <- getArgs
    return $
       case args of
        [] -> Nothing
        (filePath:_) -> loadConfig filePath