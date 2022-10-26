-- Uploader.hs
module ObjectStore
    ( Bucket (..)
    , upload
    , download
    ) where

import Data.ByteString (ByteString (..))
import Data.Int (Int64 (..))
import Data.Text (Text (..), pack)

import Network.Minio hiding (Bucket)
import Data.Conduit (ConduitT (..), yield)

data Bucket
    = Staging
    | Partitions
    deriving (Eq, Show)

packA :: Show a => a -> Text
packA = pack . show

megabytes :: Int64 -> Int64
megabytes n = n * (2^20)

upload :: Bucket -> Integer -> ByteString -> Minio ()
upload bucket partId partData =
    putObject
        (packA bucket)
        (packA partId)
        (yield partData)
        (Just $ megabytes 4)
        defaultPutObjectOptions

download :: Bucket -> Integer ->  Minio GetObjectResponse
download bucket partId =
    getObject
        (packA bucket)
        (packA partId)
        defaultGetObjectOptions
