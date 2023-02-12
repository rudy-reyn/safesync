{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SafeSync.ObjectStore where

import Control.Exception (IOException)
import Data.Char (toLower)

import Data.String (fromString)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

import Network.Minio

import Web.Scotty (status, ActionM)
import Network.HTTP.Types (Status (..))
import Network.HTTP.Client (HttpException)

import SafeSync.Database.Types

data S3Bucket
    = Partitions
    | Staging
    deriving (Eq, Show)

fromS3Bucket :: S3Bucket -> Bucket
fromS3Bucket = fromString . map toLower . show

signedUploadUrl :: S3Bucket -> UUID -> AuthToken -> Minio ByteString
signedUploadUrl bucket uuid (AuthToken authToken) =
    presignedPutObjectUrl
        (fromS3Bucket bucket)
        (uuidToText uuid)
        fourHours
        [("auth", authToken)]
  where
    fourHours :: Int
    fourHours = 4 * 60 * 60

signedDownloadUrl :: S3Bucket -> UUID -> AuthToken -> Minio ByteString
signedDownloadUrl bucket uuid (AuthToken authToken) =
    presignedGetObjectUrl
        (fromS3Bucket bucket)
        (uuidToText uuid)
        fourHours
        []
        [("auth", authToken)]
  where
    fourHours :: Int
    fourHours = 4 * 60 * 60

handleMinioErr :: MinioErr -> ActionM ()
handleMinioErr = \case
    MErrHTTP err -> handleMHttpException err
    MErrIO err -> handleMIOException err
    MErrService err -> handleMServiceErr err
    MErrValidation err -> handleMErrV err

handleMHttpException :: HttpException -> ActionM ()
handleMHttpException _ = status $ Status 500 "<HTTP>"

handleMIOException :: IOException -> ActionM ()
handleMIOException _ = status $ Status 500 "<IO>"

handleMServiceErr :: ServiceErr -> ActionM ()
handleMServiceErr =
  let resp404 = status . Status 404
  in \case
        ServiceErr code msg -> resp404 $ encodeUtf8 $ "ServiceErr " <> code <> " " <> msg
        SelectErr code msg  -> resp404 $ encodeUtf8 $ "SelectErr " <> code <> " " <> msg 
        err -> resp404 $ fromString $ show err
        -- BucketAlreadyExists -> resp404 "BucketAlreadyExists"
        -- BucketAlreadyOwnedByYou -> resp404 "BucketAlreadyOwnedByYou"
        -- NoSuchBucket -> resp404 "NoSuchBucket"
        -- InvalidBucketName -> resp404 "InvalidBucketName"
        -- NoSuchKey -> resp404 "NoSuchKey"

handleMErrV :: MErrV -> ActionM ()
handleMErrV =
  let resp404 = status . Status 404
      resp500' = status $ Status 500 "<S3>"
    -- resp500 = status . Status 500
  in \case
        -- 400s
        MErrVInvalidBucketName buc -> resp404 $ encodeUtf8 buc
        MErrVInvalidObjectName obj -> resp404 $ encodeUtf8 obj

        MErrVInvalidUrlExpiry expiry -> resp404 $ fromString $ show expiry
        MErrVSinglePUTSizeExceeded size -> resp404 $ fromString $ show size
        MErrVPutSizeExceeded size -> resp404 $ fromString $ show size
        MErrVMissingCredentials -> resp404 "Missing credentials"

        -- 500s
        _ -> resp500'
        -- MErrVRegionNotSupported _ -> resp500' -- $ encodeUtf8 $ "Region not supported: " <> reg

        -- MErrVETagHeaderNotFound -> resp500'
        -- MErrVInvalidObjectInfoResponse -> resp500'
        -- MErrVCopyObjSingleNoRangeAccepted -> resp500'
        -- MErrVInvalidHealPath -> resp500'
        -- MErrVInvalidEncryptionKeyLength -> resp500'
        -- MErrVStreamingBodyUnexpectedEOF -> resp500'
        -- MErrVUnexpectedPayload -> resp500'

        -- MErrVXmlParse _ -> resp500'
        -- MErrVJsonParse _ -> resp500'

        -- MErrVInvalidSrcObjSpec _ -> resp500'
        -- MErrVInvalidSrcObjByteRange (x, y) -> resp500'