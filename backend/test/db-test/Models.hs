-- 2023/01/13
-- Database/Models.hs

module Models where

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.IO.Class (liftIO)
import Data.Word (Word64)
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User sql=users
        Id Word64 sql=userId
        encryptedMasterKey Text
        keySalt Text
        syncTime UTCTime
        deriving Eq Show

    Partition sql=partitions
        Id Word64 sql=partitionId
        nextId PartitionId Maybe
        objectId Text
        salt Text
        checksum Text
        deriving Eq Show

    Metadata sql=metadata
        Id Word64 sql=fileId
        ownerId UserId
        partitionId PartitionId
        salt String
        path String
        modTime String
        Foreign key ownerId references User
        Foreign key partitionId references Partition
        deriving Eq Show
|]