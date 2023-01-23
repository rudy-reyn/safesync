-- 2023/01/13
-- Database/Connection.hs

module Connection where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)

import Database.Persist.Postgresql

import Models
import Queries

import Config

instance FromConfig ConnectionString where
    fromConfig config =
        "host="      ++ dbHost config ++ " "
        "port="     ++ show (dbPort config) ++ " "
        "user="     ++ dbUser config ++ " "
        "password=" ++ dbPass config ++ " "
        "dbname="   ++ dbName config

runDb :: MonadIO m => ConnectionString -> ReaderT SqlBackend m a -> m a
runDb connStr query = do
    withPostgresqlPool connStr 10 . runSqlPool query