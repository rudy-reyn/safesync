-- 2023/01/13
-- Database/Queries.hs

module Queries where

import Data.Functor ((<&>))
import Data.Time

import Database.Persist
import Database.Persist.Postgresql

import Models

getMetadata :: UserId -> [FileId] -> ReaderT SqlBackend IO [Entity Metadata]
getMetadata ownerId fileIds = selectList [MetadataOwnerId ==. ownerId, MetadataFileId <-. fileIds] []

getAllMetadata :: UserId -> Int -> Int -> ReaderT SqlBackend IO [Entity Metadata]
getAllMetadata ownerId page limit = selectList [MetadataOwnerId ==. ownerId] [OffsetBy (page*limit), LimitTo limit]

insertMetadata :: Metadata -> ReaderT SqlBackend IO FileId
insertMetadata metadata = insert metadata <&> entityKey

deleteMetadata :: [FileId] -> ReaderT SqlBackend IO ()
deleteMetadata fileIds = deleteWhere [MetadataFileId <-. fileIds]

getPartition :: PartitionId -> ReaderT SqlBackend IO (Maybe Partition)
getPartition partitionId = get partitionId

insertPartition :: Partition -> ReaderT SqlBackend IO PartitionId
insertPartition partition = insert partition <&> entityKey

deletePartition :: [PartitionId] -> ReaderT SqlBackend IO ()
deletePartition partitionIds = deleteWhere [PartitionPartitionId <-. partitionIds]

getUser :: UserId -> ReaderT SqlBackend IO (Maybe User)
getUser userId = get userId

updateSyncTime :: UserId -> ReaderT SqlBackend IO ()
updateSyncTime userId = update userId [UserSyncTime =. (UTCTime (ModifiedJulianDay 0) 0)]