{-# LANGUAGE OverloadedStrings #-}

module Server where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (getCurrentTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

import Server.Api

type AuthM = Maybe ByteString
data Success = Success
    deriving (Eq, Show)

-- Proper support for auth tokens will eventually be added.
authorize :: AuthM -> Either ServerError Success
authorize Nothing =  Left err401
authorize (Just _) = Right Success
-- authorize (Just auth) = undefined

authenticate :: AuthM -> Handler a -> Handler a
authenticate auth handler = do
    case authorize auth of
        Left err -> throwError err
        Right _ -> handler

objectEndpoint :: Integer -> AuthM -> Handler ObjectInfo
-- Will handle authorization later and database querying later
objectEndpoint id' auth = authenticate auth $ lookupObjectInfo id'
  where
--     lookupObjectInfo :: Integer -> IO ObjectInfo
    lookupObjectInfo id' = return exampleObjId

    exampleObjId :: ObjectInfo
    exampleObjId = ObjectInfo
        { objectId = id'
        , objectName = "ZGlyZWN0b3J5Cg=="
        , objectType = Directory
        , objModTime = UTCTime (fromOrdinalDate 2022 91) (secondsToDiffTime 0)
        , objectSalt = Nothing
        , objectKey  = Nothing
        , partitionIds = Just [4142, 9862, 2463]
        }

partitionEndpoint :: Integer -> AuthM -> Handler PartitionInfo
partitionEndpoint id' auth =
    authenticate auth $ return examplePartitionInfo
  where
    examplePartitionInfo :: PartitionInfo
    examplePartitionInfo =
        PartitionInfo
        { partitionId = id'
        , partitionSalt = "c2FsdAo="
        , partitionKey = "a2V5Cg=="   -- encrypted by the object symmetric key
        , nextId = Just (id' + 1)
        }

server :: Server Api
server = objectEndpoint :<|> partitionEndpoint

syncAPI :: Proxy Api
syncAPI = Proxy

app :: Application
app = server syncAPI server

main = run app 8080
