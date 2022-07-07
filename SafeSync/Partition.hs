-- 07/04/22
-- Files/Partition.hs
module SafeSync.Partitions where

import SafeSync.Files where

-- fileInfo is unpacked for the partition_journal database.
-- The remote filePartitionJournal is encrypted.
data FileEntry = {
    fileId :: String, -- I might want to use an actual ID type.
    fileVersion :: String,
    fileInfo :: FileInfo,
} deriving (Eq, Show)

data Partition = Partition {
    partitionId :: String,
    partitionFileId :: String, -- Foreign key to file entry.
    fileVersion :: String, -- If a partition is updated, the file version needs to be updated and
                           -- match between all partitions of the same file to maintain consensus.
    symmetricKey :: ByteString, -- AES-128 bit symmetric key. Needs an actual Key type.
    checksum :: ByteString, -- Uses a sha256 hash. Needs an actual digest type.

    startByte :: Int, -- Start and end byte locations of the partition
    endByte :: Int,
    partitionData :: ByteString
} deriving (Eq, Show)

data PartitionJournal = PartitionJournal {
    journalFiles :: FileEntry,
    journalPartitions :: [Partition],
} deriving (Eq, Show)

