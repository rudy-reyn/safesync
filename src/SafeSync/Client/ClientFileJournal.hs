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
-- Client/ClientFileJournal.hs
import Data.Time (UTCTime)
import Data.ByteString (ByteString)

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Metadata
        file_id String
        path String
         -- 'True' for regular file, 'False' for directory, 'null' for unsupported
        file_type Bool Maybe
        modification_time UTCTime

        Primary file_id
        UniquePath path
        deriving Eq Show

    Partition sql=partitions
        partition_id String
        salt ByteString
        -- Only decrypted while in memory
        symmetric_key ByteString

        -- FK, not synced
        file_id String Maybe
        -- Encrypted and converted to a ByteString before being synced with the backend
        next_id String Maybe
        checksum String Maybe

        Primary partition_id
        Foreign Metadata fk_partition_metadata file_id
        deriving Eq Show
|]
