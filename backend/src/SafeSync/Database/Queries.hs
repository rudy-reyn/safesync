-- 2023/01/13
-- Database/Queries.hs

module SafeSync.Database.Queries where

import Data.Functor ((<&>))
import Data.Time

import Database.Persist
import Database.Persist.Postgresql
import Database.Esqueleto.Experimental (withRecursive)

import SafeSync.Database.Models

type DatabaseReader a = ReaderT SqlBackend IO a

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

getFilePartitionIds :: FileId -> ReaderT SqlBackend IO (Maybe [PartitionId])
getFilePartitionIds partitionId =
    select $ from $ do
        partitionIds <- withRecursive
            (select $ do
                (PartitionId :& PartitionNextId) <- do
                    from (partitions `InnerJoin` metadata)
                where_ (partitions ^. PartitionId ==. metadata PartitionId &&. metadata ^. FileId ==. 1)
            )
            unionAll_
            (\self -> do
                select_ (partitions ^. PartitionId, :& partitions ^. nextId)
                from (partitions `InnerJoin` self)
                where_ (self ^. NextId ==. partitions ^. PartitionId))
         partitionIds ^. PartitionId

    -- WITH RECURSIVE partitionIds as (
    -- 		SELECT partitions.partitionId, partitions.nextId
    -- 		FROM partitions, metadata
    -- 		WHERE partitions.partitionId = metadata.partitionId AND metadata.fileId = 1
    -- 		UNION ALL
    -- 		SELECT partitions.partitionId, partitions.nextId
    -- 		FROM partitions, partitionIds
    -- 		WHERE partitionIds.nextId = partitions.partitionId
    -- 	)

    -- SELECT partitionId FROM partitionIds;

insertPartition :: Partition -> ReaderT SqlBackend IO PartitionId
insertPartition partition = insert partition <&> entityKey

deletePartition :: [PartitionId] -> ReaderT SqlBackend IO ()
deletePartition partitionIds = deleteWhere [PartitionPartitionId <-. partitionIds]

getUser :: UserId -> ReaderT SqlBackend IO (Maybe User)
getUser userId = get userId

updateSyncTime :: UserId -> ReaderT SqlBackend IO ()
updateSyncTime userId = update userId [UserSyncTime =. (UTCTime (ModifiedJulianDay 0) 0)]