{-# LANGUAGE OverloadedStrings #-}
module SafeSync.ObjectStore.Object where

import Data.String (IsString (..))
import Data.Char (toLower)

import Network.Minio

data S3Bucket
    = Partitions | Staging
    deriving Eq

partitions :: S3Bucket
partitions = Partitions

staging :: S3Bucket
staging = Staging

instance Show S3Bucket where
    show Staging = "staging"
    show _ = "partitions"

instance IsString S3Bucket where
    fromString bucket =
      case map toLower bucket of
        "staging" -> Staging
        "partitions" -> Partitions
        _notABucket -> error "Must be either \"staging\" or \"partitions\""