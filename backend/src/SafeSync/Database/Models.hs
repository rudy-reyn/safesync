{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module SafeSync.Database.Models where

import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import Database.Persist.TH
    ( mkMigrate,
      mkPersist,
    -- persistUpperCase,
      persistLowerCase,
      share,
      sqlSettings )

import SafeSync.Database.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json sql=users
        userId UUID sqltype=uuid
        email Email sqltype=varchar(320)
        phone Phone sqltype=character(15)
        password Password sqltype=character(32)
        passwordSalt Salt sqltype=character(16)
        masterKey Key sqltype=character(32)
        keySalt Salt sqltype=character(16)
        syncTime UTCTime
        deriving Eq Show

    Partition json sql=partitions
        partitionId UUID Unique sqltype=uuid
        objectId UUID sqltype=uuid
        nextId UUID Maybe Unique sqltype=uuid
        salt Salt sqltype=character(16)
        checksum Text
        createdAt UTCTime sqltype=timestamp
        updatedAt UTCTime Maybe sqltype=timestamp
        deriving Eq Show

    Object json sql=objects
        objectId UUID Unique sqltype=uuid
        ownerId UUID sqltype=uuid
        headId UUID Maybe Unique sqltype=uuid
        uploadedAt UTCTime
        salt Salt sqltype=character(16)
        path Text
        modTime UTCTime Maybe
        deriving Eq Show
|]
