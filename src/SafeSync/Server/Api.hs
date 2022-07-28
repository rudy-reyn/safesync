-- Api.hs
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
    { userId :: Int
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data UploadInfo = UploadInfo
    { numPartitions :: Int,
    , lastPartitionSize :: Int,
    , partitionIds :: [String]
    } deriving (Eq, Show)
$(deriveJSON defaultOptions ''UploadInfo)

type SyncApi = "sync" :> Get '[JSON] [User]

