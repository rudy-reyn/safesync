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

module Models where

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

data ObjType = File | Directory
    deriving (Eq, Read, Show)

data ObjStatus = Active | Deleted
    deriving (Eq, Read, Show)

type Encrypted = ByteString

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User sql=users
        email    Text
        salt     ByteString
        password Text
        UniqueUserEmail email
        deriving Eq Show

    -- | Linking table between users and objects
    AccessList sql=accessList
        userId   UserId
        objectId ObjectId
        status   ObjStatus
        updated  UTCTime
        Primary  userId objectId
        deriving Eq Show

    Object sql=objects
        salt    ByteString
        type    ObjType
        name    Encrypted         -- | File name
        modTime Encrypted         -- | Last modification time name
        key     Encrypted         -- | Symmetric key
        head    PartitionId Maybe -- | First partition, null for directories
        deriving Eq Show

    Partition sql=partitions
        objectStoreId Varchar        -- | Location in the file object store
        salt ByteString
        key  Encrypted
        next PartitionId Maybe
        deriving Eq Show
|]
