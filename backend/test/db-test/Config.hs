-- 2023/01/13
-- Config.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import GHC.Generics
import Data.ByteString.Lazy (readFile)
import System.Environment (getArgs)
import Data.Word (Word16)

import Data.Aeson

class FromConfig a where
    fromConfig :: Config -> a

data Config = Config {
    dbHost :: String,
    dbPort :: Int,
    dbUser :: String,
    dbPass :: String,
    dbName :: String,
    serverPort :: Word16
} deriving (Show, Generic)

instance FromJSON Config

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig filePath = do
    configData <- readFile filePath
    return $ decode configData

fromArgv :: IO (Maybe Config)
fromArgv = do
    args <- getArgs
    return $
       case args of
        [] -> Nothing
        (filePath:_) -> loadConfig filePath