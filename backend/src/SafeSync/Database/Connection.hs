-- 2023/01/13
-- Database/Connection.hs

module SafeSync.Database.Connection where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)

import Database.Persist.Postgresql

import SafeSync.Database.Models
import SafeSync.Database.Queries
import SafeSync.Config

instance FromConfig ConnectionString where
    fromConfig (Config _ _ config _) = 
        "host="      ++ dbHost config ++
        " port="     ++ show (dbPort config) ++
        " user="     ++ dbUser config ++
        " password=" ++ dbPass config ++
        " dbname="   ++ dbName config

runDb :: MonadIO m => ConnectionString -> ReaderT SqlBackend m a -> m a
runDb connStr query = do
    withPostgresqlPool connStr 10 $ runSqlConn query