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

$(deriveJSON defaultOptions ''User)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

server :: Server API
server = return users

api :: Proxy API
api = Proxy

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
