{-# LANGUAGE OverloadedStrings #-}
module SafeSync.ObjectStore.Events where

import Network.Minio
import SafeSync.Database.Types (UUID, uuidToText)

partitionsDestInfo :: DestinationInfo
partitionsDestInfo = defaultDestinationInfo { dstBucket = "partitions" }

stagingSourceInfo :: SourceInfo
stagingSourceInfo = defaultSourceInfo { srcBucket = "staging" }

unstagePartition :: UUID -> Minio ()
unstagePartition partitionId = do
    copyObject (partitionsDestInfo {dstObject = uuidToText partitionId} )
               (stagingSourceInfo  {srcObject = uuidToText partitionId} )

removeStagedPartition :: UUID -> Minio ()
removeStagedPartition = removeObject "staging" . uuidToText

unstageObjectPartitions :: [UUID] -> Minio ()
unstageObjectPartitions partitionIds = do
    mapM_ unstagePartition partitionIds
    mapM_ removeStagedPartition partitionIds
