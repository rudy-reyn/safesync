-- Server/MetadataDatabase.hs
module SafeSync.Server.MetadataDatabase where

import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Postgresql
import SafeSync.Server.ORM

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
    Partition
        partition_id Int
        user_id Int
        partition_salt ByteString
        symmetric_key ByteString
        next_id ByteString Maybe
        path ByteString
        file_type Bool
        size ByteString Maybe
        modification_time ByteString
        checksum ByteString
        deriving Eq Show
    SyncTime
        user_id Int
        sync_time
        deriving Eq Show
|]
