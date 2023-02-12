{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module SafeSync.Database.Queries where

import Data.Time.Clock (UTCTime, getCurrentTime)

import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO )

import Database.Esqueleto.Experimental

import SafeSync.Database.Models
import qualified SafeSync.Database.Models as Models
import SafeSync.Database.Types

type Select m backend a b =
    ( MonadIO m, BackendCompatible SqlBackend backend
    , PersistUniqueRead backend, PersistQueryRead backend)
    => a -> ReaderT backend m b

type Update m backend a b =
    ( MonadIO m, BackendCompatible SqlBackend backend
    , PersistQueryWrite backend, PersistUniqueWrite backend)
    => a -> ReaderT backend m b

type Insert m backend a record =
    (MonadIO m, SafeToInsert record
    , PersistRecordBackend record backend)
    => a -> ReaderT backend m (Maybe (Models.Key record))

selectUserObjectIds :: Select m backend UUID [Value UUID]
selectUserObjectIds userId =
    select $ do
        obj <- from $ table @Object 
        where_ $ obj ^. ObjectHeadId ==. val (Just userId)
        return (obj ^. ObjectObjectId)

selectPartition :: Select m backend UUID (Maybe (Entity Partition))
selectPartition partId =
    selectOne $ do
        part <- from $ table @Partition 
        where_ $ part ^. PartitionPartitionId ==. val partId
        return part

selectObjectPartitions :: Select m backend UUID [Entity Partition]
selectObjectPartitions objectId =
    select $ do
        part <- from $ table @Partition 
        where_ $ part ^. PartitionObjectId ==. val objectId
        return part

selectObjectPartitionIds :: Select m backend UUID [Value UUID]
selectObjectPartitionIds objectId =
    select $ do
        obj <- from $ table @Partition 
        where_ $ obj ^. PartitionObjectId ==. val objectId
        return (obj ^. PartitionPartitionId)

selectPartitionObjectId :: Select m backend UUID (Maybe (Value UUID))
selectPartitionObjectId partId =
    selectOne $ do
        obj <- from $ table @Partition 
        where_ $ obj ^. PartitionPartitionId ==. val partId
        return (obj ^. PartitionObjectId)

selectObject :: Select m backend UUID (Maybe (Entity Object))
selectObject objectId = 
    selectOne $ do
    obj <- from $ table @Object 
    where_ $ obj ^. ObjectObjectId ==. val objectId
    return obj

insertPartition :: PersistUniqueWrite backend => Insert m backend Partition Partition
insertPartition = insertUnique 

insertObject :: PersistUniqueWrite backend => Insert m backend Object Object
insertObject = insertUnique 

insertUser :: PersistUniqueWrite backend => Insert m backend User User
insertUser = insertUnique 

    -- selectOne $ do
    --     part <- from $ table @Partition 
    --     where_ $ part ^. PartitionPartitionId ==. val partId
    --     return part

updateObjectHeadId :: Update m backend (UUID, UUID) ()
updateObjectHeadId (partId, nextId) =
    update $ \field -> do
    set field [ObjectHeadId =. just (val nextId)]
    where_ $ field ^. ObjectObjectId ==. val partId

updateObjectModTime :: Update m backend (UUID, UTCTime) ()
updateObjectModTime (partId, nextId) =
    update $ \field -> do
    set field [ObjectModTime =. just (val nextId)]
    where_ $ field ^. ObjectObjectId ==. val partId

updatePartitionNextId :: Update m backend (UUID, UUID) ()
updatePartitionNextId (partId, nextId) =
    update $ \field -> do
    set field [PartitionNextId =. just (val nextId)]
    where_ $ field ^. PartitionPartitionId ==. val partId

updatePartitionUpdatedAt :: Update m backend (UUID, UTCTime) ()
updatePartitionUpdatedAt (partId, time) =
    update $ \field -> do
    set field [PartitionUpdatedAt =. just (val time)]
    where_ $ field ^. PartitionPartitionId ==. val partId

updateUserSyncTime :: Update m backend (UUID, UTCTime) ()
updateUserSyncTime (partId, time) =
    update $ \field -> do
    set field [UserSyncTime =. val time]
    where_ $ field ^. UserUserId ==. val partId