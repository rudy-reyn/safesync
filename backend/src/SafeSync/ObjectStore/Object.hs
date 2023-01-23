{-# LANGUAGE OverloadedStrings #-}
module SafeSync.ObjectStore.Object where

import Data.Text (toLower)

import Network.Minio

import SafeSync.Utils ( IsText(..) )
import SafeSync.Config

data S3Bucket
    = Partitions | Staging
    deriving (Eq, Show)

instance FromConfig ConnectInfo where
  fromConfig (Config _ _ _ config) =
    let connInfo :: ConnectInfo
        connInfo = s3Host config ++ ":" ++ show (s3Port config)
        creds = Credentials (s3AccessKey config) (s3SecretKey config)
        region = fromMaybe "" (s3Region config)
    in
    setCreds creds $
        setRegion region connInfo 

connInfoFromConfig :: Config -> ConnectInfo 
connInfoFromConfig = fromConfig

instance IsText S3Bucket where
    toText Staging = "staging"
    toText _ = "partitions"

    fromText text =
        case toLower text of
            "staging" -> Staging
            "partitions" -> Partitions
            _notABucket -> error "Invalid bucket name"