{-# LANGUAGE MagicHash #-}
-- Api.hs
module SafeSync.Server.Api where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)

type UserId = String

data UploadInfo
    = UploadInfo {
        uploadUserId :: UserId,
        uploadStartTime :: UTCTime,
        numPartitions :: Int,
        lastPartitionSize :: Int,
        partitionIds :: [String]
    } deriving (Eq, Show)

-- Everything suffixed with # is encrypted with the symmetric key
-- The symmetric key is encrypted with the users master key

data Partition
    = Partition {
        partitionId String,
        salt ByteString,
        symmetricKey# ByteString,
        path# (Maybe ByteString),
        modificationTime# (Maybe ByteString),
        nextId# (Maybe ByteString),
        checksum# (Maybe ByteString)
    } deriving (Eq, Show)

