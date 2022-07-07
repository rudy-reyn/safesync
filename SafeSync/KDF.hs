{-# LANGUAGE OverloadedStrings #-}
-- 07/02/22
-- KDF.hs
module SafeSync.KDF (
        Key, Salt, defaultOptions,
        argon2i, argon2i', argon2iS, argon2iS',
        sha256, sha256b, sha256s
    ) where

import Data.Word (Word32, Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS

import Control.Exception (throw)
import Crypto.Error (throwCryptoError, CryptoFailable (..))

import Data.ByteArray (ByteArray, Bytes)
import qualified Data.ByteArray as BA

import qualified Crypto.Hash as Hash
import qualified Crypto.KDF.Argon2 as Argon2

import Crypto.Random (getRandomBytes)

newtype Key = Key Bytes deriving (Eq, Show)
newtype Salt = Salt Bytes deriving (Eq, Show)

generateSalt :: IO Bytes
generateSalt = getRandomBytes 32

-- These are arbitrarily selected and should instead be derived from computer architecture
defaultOptions = Argon2.Options {
    Argon2.memory = gbToKiB 1,
    Argon2.iterations = 10::Word32,
    Argon2.parallelism = 4::Word32,
    Argon2.variant = Argon2.Argon2i,
    Argon2.version = Argon2.Version13
}

argon2i :: Bytes -> Bytes -> CryptoFailable Bytes
argon2i password salt = Argon2.hash defaultOptions password salt 256

argon2i' :: Bytes -> Bytes -> Bytes
argon2i' password salt
    = case argon2i password salt of
        CryptoPassed enc -> enc
        CryptoFailed err -> throw err

argon2iBS :: ByteString -> ByteString -> CryptoFailable Bytes
argon2iBS password salt = argon2i (byteStrToBytes password) (byteStrToBytes salt)

argon2iBS' :: ByteString -> ByteString -> Bytes
argon2iBS' password salt =  argon2i' (byteStrToBytes password) (byteStrToBytes salt)

argon2iS :: String -> String -> CryptoFailable Bytes
argon2iS password salt = argon2i (strToBytes password) (strToBytes salt)

argon2iS' :: String -> String -> Bytes
argon2iS' password salt =  argon2i' (strToBytes password) (strToBytes salt)

sha256 :: ByteString -> Hash.Digest Hash.SHA256
sha256 = Hash.hashWith Hash.SHA256

sha256b :: Bytes -> Hash.Digest Hash.SHA256
sha256b = sha256 . bytesToByteStr

sha256s :: String -> Hash.Digest Hash.SHA256
sha256s = sha256 . Char8.pack

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

strToWord8 :: String -> [Word8]
strToWord8 = map charToWord8

byteStrToWord8 :: ByteString -> [Word8]
byteStrToWord8 = BS.unpack

strToBytes ::  String -> Bytes
strToBytes = BA.pack . strToWord8

bytesToByteStr :: Bytes -> ByteString
bytesToByteStr = BS.pack . BA.unpack

byteStrToBytes :: ByteString -> Bytes
byteStrToBytes = BA.pack . byteStrToWord8

gbToKiB :: Word32 -> Word32
gbToKiB = (* 976562)

main = do
    let salt = "salt salt"
        password = "password"
        passwordHash = sha256s password
        saltedHash = sha256s (password ++ salt)
        derived  = argon2iS' password salt
        derivedHash = sha256b derived
        println = putStrLn

    println $ "salt = " ++ show salt
    println $ "password = " ++ show password
    println $ "derived = " ++ show derived
    println $ "passwordHash = " ++ show passwordHash
    println $ "saltedHash = " ++ show saltedHash
    println $ "derivedHash = " ++ show derivedHash
    print $ argon2iBS' ("password"::ByteString) ("salt salt"::ByteString)
