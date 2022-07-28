-- Api.hs
module SafeSync.Server.Api where

data Metadata

data Upload = Upload UserId UploadInfo
    deriving (Eq, Show)

data UploadInfo = UploadInfo
    { numPartitions :: Int,
    , lastPartitionSize :: Int,
    , partitionIds :: [String]
    } deriving (Eq, Show)

