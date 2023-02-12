{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SafeSync.Database.Types 
    ( UUID (..)
    , ObjectType (..)
    , Character (..)
    , VarChar (..)
    , Salt (..)
    , Key (..)
    , Password (..)
    , Email (..)
    , Phone (..)
    , AuthToken (..)
    , IsText (..)
    , uuidToText
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.ByteString as BS
import Data.String (IsString (..))

import Web.Scotty (Parsable (..))
import Database.Persist.Class (PersistField (..))
import Database.Persist.PersistValue (PersistValue (..), LiteralType (Escaped))
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson.Types (Value (..), prependFailure, typeMismatch)

import Data.UUID (UUID (..))
import qualified Data.UUID as U
import GHC.TypeLits (Nat, natVal, KnownNat)

newtype Character (size::Nat) where
    Character :: Text -> Character size
    deriving (Eq, Show, ToJSON, IsString) via Text

newtype VarChar (size::Nat) where
    VarChar :: Text -> VarChar size
    deriving (Eq, Show, ToJSON, IsString) via Text

class IsText a where
    fromText :: Text -> a
    fromTextMaybe :: Text -> Maybe a
    fromTextEither :: Text -> Either String a

    fromText a =
        case fromTextEither a of
            Right a' -> a'
            Left err -> error err
    fromTextMaybe a =
        case fromTextEither a of
            Left _ -> Nothing
            Right a' -> Just a'

instance  IsText Text

instance KnownNat size => IsText (Character (size::Nat)) where
    fromTextEither text
            | len == len' = Right char
            | otherwise = Left $ "Character of length " <> show len
                            <> " but should be of length " <> show len'
        where char = Character text
              len = T.length text
              len' = fromInteger $ natVal char

instance KnownNat size => IsText (VarChar (size::Nat)) where
    fromTextEither text
        | len <= len' = Right char
        | otherwise = Left $ "Character of length " <> show len
                         <> " but less than or equal to " <> show len'
        where char = VarChar text
              len = T.length text
              len' = fromInteger $ natVal char

-- Both of these need their own validators and parsers but this should work for now
newtype Email where
    Email :: Text -> Email
    deriving (Eq, Show)
    deriving (FromJSON, ToJSON, IsText, IsString, Parsable, PersistField)
        via VarChar 320

newtype Phone where
    Phone :: Text -> Phone
    deriving (Eq, Show)
    deriving (FromJSON, ToJSON, IsText, IsString, Parsable, PersistField)
        via Character 15

newtype Password where
    Password :: Text -> Password
    deriving (Eq, Show)
    deriving (FromJSON, ToJSON, IsText, IsString, Parsable, PersistField)
        via Character 32

newtype Salt where
    Salt :: Text -> Salt
    deriving (Eq, Show)
    deriving (FromJSON, ToJSON, IsText, IsString, Parsable, PersistField)
        via Character 16

newtype Key where
    Key :: Text -> Key
    deriving (Eq, Show)
    deriving (FromJSON, ToJSON, IsText, IsString, Parsable, PersistField)
        via Character 32

newtype AuthToken where
    AuthToken :: ByteString -> AuthToken
    deriving (Eq, Show)
    deriving (IsString) via ByteString

data ObjectType
    = File | Directory
    deriving (Eq, Show)

instance KnownNat size => PersistField (Character (size::Nat)) where
    toPersistValue (Character text) = PersistText text
    fromPersistValue (PersistText text) =
        case fromTextEither text of 
            Right char -> Right char
            Left err -> Left (fromString err)
    fromPersistValue _ = Left "fromPersistValue: No Conversion"

instance KnownNat size => PersistField (VarChar (size::Nat)) where
    toPersistValue (VarChar text) = PersistText text
    fromPersistValue (PersistText text) =
        case fromTextEither text of 
            Right char -> Right char
            Left err -> Left (fromString err)
    fromPersistValue _ = Left "fromPersistValue: No Conversion"

instance KnownNat size => Parsable (Character (size::Nat)) where
    parseParam textL = 
        case fromTextEither $ TL.toStrict textL of
            Right char -> Right char
            Left err -> Left $ fromString err

instance KnownNat size => Parsable (VarChar (size::Nat)) where
    parseParam textL = 
        case fromTextEither $ TL.toStrict textL of
            Right char -> Right char
            Left err -> Left $ fromString err

instance KnownNat size => FromJSON (Character (size::Nat)) where
    parseJSON (String string) =
        case fromTextEither string of
            Right char -> return char
            Left err -> prependFailure (fromString err) (typeMismatch "Invalid base64" $ String string)
    parseJSON invalid = prependFailure "parsing Character failed, expected String" (typeMismatch "String" invalid)

instance KnownNat size => FromJSON (VarChar (size::Nat)) where
    parseJSON (String string) =
        case fromTextEither string of
            Right char -> return char
            Left err -> prependFailure (fromString err) (typeMismatch "Invalid base64" $ String string)
    parseJSON invalid = prependFailure "parsing Character failed, expected String" (typeMismatch "String" invalid)

uuidToText :: UUID -> Text
uuidToText = U.toText

instance Parsable ObjectType where
    parseParam = \case
        "0" -> pure Directory
        "d" -> pure Directory
        "dir" -> pure Directory
        "directory" -> pure Directory
        "1" -> pure File
        "f" -> pure File
        "file" -> pure File
        _ -> Left "Invalid ObjectType"

instance PersistField UUID where
    toPersistValue = PersistLiteral_ Escaped . U.toASCIIBytes
    fromPersistValue = \case
        PersistText text -> mk U.fromText text
        PersistLiteral_ _ bytes -> mk U.fromASCIIBytes bytes
        PersistByteString bytes -> mk U.fromASCIIBytes bytes
        _ -> Left "fromPersistValue: No Conversion"
      where
       mk :: (a -> Maybe UUID) -> a -> Either Text UUID
       mk f a = case f a of
            Nothing -> Left "fromPersistValue: Invalid UUID"
            Just uuid -> Right uuid

instance Parsable UUID where
    parseParam text = 
       case U.fromText $ TL.toStrict text of
            Nothing -> Left "parseParam: Invalid UUID"
            Just uuid -> Right uuid

instance IsString UUID where
    fromString string =
        case U.fromString string of
            Just uuid -> uuid
            Nothing -> error "fromString: Invalid UUID"