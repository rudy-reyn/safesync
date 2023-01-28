-- 2023/01/13
-- SafeSync/Config.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SafeSync.Config where

import GHC.Generics
import Data.Word (Word16)

import Data.Aeson

type Port = Word16

data Config = Config {
    serverConfig :: ServerConfig,
    dbConfig :: DBConfig,
    s3Config :: S3Config
} deriving (Show, Generic)

data ServerConfig = ServerConfig {
    serverHost :: String,
    serverPort :: Port,
    serverIsVerbose :: Maybe Bool
    serverIsSecure :: Maybe Bool
} deriving (Show, Generic)

data DBConfig = DBConfig {
    dbHost :: String,
    dbPort :: Port,
    dbUser :: String,
    dbPass :: String,
    dbName :: String
} deriving (Show, Generic)

data S3Config = S3Config {
    s3Host :: String,
    s3Port :: Port,
    s3IsSecure :: Maybe Bool,
    s3AccessKey :: Maybe String,
    s3SecretKey :: Maybe String,
    s3Region :: Maybe String
} deriving (Show, Generic)

class FromConfig a where
    fromConfig :: Config -> a

instance FromConfig ServerConfig where
    fromConfig = serverConfig

instance FromConfig DBConfig where
    fromConfig = dbConfig

instance FromConfig S3Config where
    fromConfig = s3Config

instance FromJSON ServerConfig
instance FromJSON DBConfig
instance FromJSON S3Config
instance FromJSON Config