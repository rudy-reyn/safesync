{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
-- Database/Models.hs
module SafeSync.Database.Models where

import Data.Word (Word64)
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Persist
import Database.Persist.TH

type UUID = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    Id Word64
    masterKey Text
    keySalt Text
    syncTime UTCTime
    deriving Eq Show

Partition sql=partitions
    Id UUID
    nextId PartitionId Maybe
    salt Text
    checksum Text
    deriving Eq Show

Metadata
    Id Word64
    ownerId UserId
    partitionId PartitionId Maybe
    salt Text
    path Text
    modTime Text
    deriving Eq Show

File sql=partitions
    Id MetadataId
    ownerId UserId
    salt Text
    path Text
    modTime Text
    partitionIds [UUID]
    deriving Eq Show
|]
