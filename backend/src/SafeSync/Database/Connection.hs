-- 2023/01/13
-- Database/Connection.hs

module SafeSync.Database.Connection where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)

import Database.Persist.Postgresql

import SafeSync.Database.Models
import SafeSync.Database.Queries

newtype DBInfo = DInfo {dbInfo :: ConnectionString}
    deriving (Eq, Show)

runDb :: MonadIO m => DBInfo -> ReaderT SqlBackend m a -> m a
runDb (DBInfo connStr) query =
    withPostgresqlPool connStr 10 $ runSqlConn query