module SafeSync.Auth where

import Control.Monad.IO.Class
import Data.ByteString (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8, Word64)

import System.Random

import SafeSync.Database.Types

randomRIOs :: (Random a, MonadIO m) => Word -> (a, a) -> m [a]
randomRIOs size range = go size []
  where
    go 0 xs = return xs
    go n xs = do
        x <- randomRIO range
        go (n - 1) (x:xs)

-- uuidV4 :: IO UUID
-- uuidV4 = do
--     a <- randomRIO (minBound, maxBound)
--     b <- randomRIO (minBound, maxBound)
--     return $ UUID a b

-- ED781A3B-D7A5-4E50-9148-9B737C9FDFA4

genAuthToken128 :: IO AuthToken
genAuthToken128 = do
    token <- randomRIOs 16 (65, 122) :: IO [Word8]
    return $ AuthToken $ pack token
