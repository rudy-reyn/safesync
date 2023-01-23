{-# LANGUAGE OverloadedStrings #-}
-- 2023/01/13
-- ObjectStore/S3.hs

module SafeSync.ObjectStore.S3 where

import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.Text (Text)

import Network.Minio

import SafeSync.Config
import SafeSync.ObjectStore.Object

moveFromStaging :: Object -> Minio ()
moveFromStaging object = Destin

stagingDestInfo :: DestinationInfo
stagingDestInfo =
    defaultDestinationInfo {dstBucket = toText Staging}

partitionsDestInfo :: DestinationInfo
partitionsDestInfo =
    defaultDestinationInfo {dstBucket = toText Partitions}

newDownloadUrl :: S3Bucket -> Object -> Minio ByteString
newDownloadUrl bucket object =
    presignedGetObjectUrl (toText bucket) object 3600 []

newUploadUrl :: S3Bucket -> Object -> Minio ByteString
newUploadUrl bucket object =
    presignedPutObjectUrl (toText bucket) object 3600 [] []

getObjectInfo :: S3Bucket -> Object -> Minio ObjectInfo
getObjectInfo object =
    statObject (toText bucket) object defaultGetObjectOptions