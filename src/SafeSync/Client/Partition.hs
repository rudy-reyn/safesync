module SafeSync.Client.Partitions where

import SafeSync.Client.Files where

-- fileInfo is unpacked for the partition_journal database.
-- The remote filePartitionJournal is encrypted.
data FileEntry = {
    fileId :: String, -- I might want to use an actual ID type.
    fileInfo :: FileInfo,
} deriving (Eq, Show)

data Partition = Partition {
    partitionId :: String,
    symmetricKey :: ByteString, -- AES-128 bit symmetric key. Needs an actual Key type.
    checksum :: ByteString, -- Uses a sha256 hash. Needs an actual digest type.
    partitionData :: ByteString
} deriving (Eq, Show)

data PartitionJournal = PartitionJournal {
    journalFiles :: FileEntry,
    journalPartitions :: [Partition],
} deriving (Eq, Show)

