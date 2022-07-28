{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE #-}
-- {-# LANGUAGE #-}
-- {-# LANGUAGE #-}
-- ClientFileJournal.hs
module ClientFileJournal where

import qualified Database.Esqueleto as Esql
import qualified Database.Persist as Persist
import qualified Database.Persist.TH

import SafeSync.Client.Files (FileType)

type Timestamp = String

share [Persist.mkPersist Persist.sqlSettings, mkMigrate "migrateAll"] [Persist.persistLowerCase|
   FileSystem
        Path String
        file_type FileType -- Directory | RegularFile | Unsupported
        deriving (Eq, Show)
|]
