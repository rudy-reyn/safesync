{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.ByteString (ByteString)

import GHC.Generics

import Data.Aeson
import Servant.API

type Base64 = Text  -- A base64 encoded ByteString
type EncBase64 = Base64 -- Encrypted Base64

type Api = ObjectEndpoint :<|> PartitionEndpoint

type Authorization = Header "Authorization" Text

type ObjectEndpoint
    = "object"
    :> Capture "fileId" Integer
    :> Authorization
    :> Get '[JSON] ObjectInfo

type PartitionEndpoint
    = "partition"
    :> Capture "partitionId" Integer
    :> Authorization
    :> Get '[JSON] PartitionInfo
    -- Typically a request for both will be sent at once by the client.
--     :> ("info" :> Get '[JSON] PartitionInfo
--              :<|> Get '[OctetStream] ByteString)

data ObjectInfo
    = ObjectInfo
    { objectId :: Integer
    , objectName :: EncBase64 -- encrypted by object symmetric key
    , objectType :: ObjType
    , objModTime :: UTCTime
    , objectSalt :: Maybe Base64
    , objectKey  :: Maybe EncBase64 -- encrypted by the users master key
    , partitionIds :: Maybe [Integer]
    } deriving (Eq, Generic, Show)

instance ToJSON   ObjectInfo
instance FromJSON ObjectInfo

data PartitionInfo
    = PartitionInfo
    { partitionId   :: Integer
    , partitionSalt :: Base64
    , partitionKey  :: EncBase64   -- encrypted by the object symmetric key
    , nextId :: Maybe Integer
    } deriving (Eq, Generic, Show)

instance ToJSON   PartitionInfo
instance FromJSON PartitionInfo

data ObjType = File | Directory
    deriving (Eq, Generic, Show)

instance ToJSON ObjType
instance FromJSON ObjType

newtype Partition
    = Partition { partitionData :: ByteString }  -- encrypted by partition symmetric key
    deriving (Eq, Show)

newtype AuthToken
    = AuthToken { authToken :: ByteString }
    deriving (Eq, Show)
