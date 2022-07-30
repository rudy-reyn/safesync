{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- Server/MetadataDatabase.hs
module SafeSync.Server.MetadataDatabase where

import Data.Time (UTCTime)
import Data.ByteString (ByteString)

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Partition sql=partitions
        partition_id String
        user_id Int
        salt ByteString
        symmetric_key ByteString
        next_id ByteString Maybe
        checksum ByteString Maybe

        Primary partition_id
        Foreign SyncTimes fk_partition_sync_times user_id
        Foreign Metadata fk_partition_metadata partition_id
        deriving Eq Show
    Metadata
        partition_id String
        path ByteString
        modification_time ByteString Maybe

        Primary partition_id
        Foreign Partition fk_metadata_partition partition_id
        deriving Eq Show

    SyncTimes sql=sync_times
        user_id Int
        last_sync_time UTCTime
        Primary user_id
        deriving Eq Show
|]
