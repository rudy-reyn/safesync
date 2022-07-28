-- 07/27/22
-- Partitions.hs
module Partitions (EncryptedPartition) where

import Data.ByteString (ByteString)

-- Everything prefixed with ' is encrypted with the symmetric key
-- The symmetric key is encrypted with the users master key

data EncryptedPartition
    = EncryptedPartition
   , 'metadata :: EncryptedMetadata
    } deriving (Eq, Show)

