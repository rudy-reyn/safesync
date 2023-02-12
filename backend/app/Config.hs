{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
-- app/Config.hs
-- safesync.io
module Config where

import GHC.Generics (Generic)
import Data.Word (Word16)
import Data.String (IsString (..))

import Data.Default.Class
import Data.Aeson
import Database.Persist.Postgresql (ConnectionString)

type Port = Word16
type Host = String

data Config
    = Config {
        serverConfig :: ServerConfig,
        dbConfig :: DBInfo,
        s3Config :: S3Config
    } deriving (Eq, Show, Generic)

data ServerConfig
    = ServerConfig {
        serverPort :: Port,
        verbose :: Bool,
        timeout :: Int
    } deriving (Eq, Show, Generic)

data S3Config
    = S3Config {
        s3Port :: Port,
        s3Host :: Host,
        s3AccessKey :: Maybe String,
        s3SecretKey :: Maybe String,
        s3Region :: Maybe String
    } deriving (Eq, Show, Generic)

newtype DBInfo = DBInfo ConnectionString
    deriving (Eq, Show)
    deriving IsString via ConnectionString

instance Default DBInfo where
    def = DBInfo "dbport=5432 dbhost=localhost"

defDBInfo :: DBInfo
defDBInfo = def

defServerConfig :: ServerConfig
defServerConfig = def

defS3Config :: S3Config
defS3Config = def

-- =================
-- FromJSON Configs
-- =================
instance FromJSON ServerConfig where
    parseJSON = withObject "ServerConfig" $ \v ->
      ServerConfig
        <$> v .:? "port" .!= 8080
        <*> v .:? "verbose" .!= False
        <*> v .:? "timeout" .!= 30

instance FromJSON S3Config where
    parseJSON = withObject "S3Config" $ \v ->
      S3Config
        <$> v .:? "port" .!= 9000
        <*> v .:? "host" .!= "http://localhost"
        <*> v .:? "accessKey"
        <*> v .:? "secretKey"
        <*> v .:? "region"

-- =================
-- Default Configs
-- =================
instance Default Config where
    def = Config def def def

instance Default ServerConfig where
    def = ServerConfig {
            serverPort = 8080,
            verbose = False,
            timeout = 30
        }

instance Default S3Config where
    def = S3Config {
            s3Port = 9000,
            s3Host = "http://localhost",
            s3AccessKey = Nothing,
            s3SecretKey = Nothing,
            s3Region = Nothing
        }
