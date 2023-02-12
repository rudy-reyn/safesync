{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module SafeSync.Database.Types.Test.Test (testTypes) where

import Data.String (fromString)
import Test.HUnit ( (@=?), Test, Testable(test), Assertion )
import SafeSync.Database.Types 

testTypes :: Test
testTypes = test
    [ testIsTextCharacter
    , testIsTextVarChar
    , testIsKey ]

testIsTextCharacter :: [Assertion]
testIsTextCharacter =
    let ftm5 a = fromTextMaybe a :: Maybe (Character 5)
    in [ Nothing @=? ftm5 ""
       , Just (Character "abcde") @=? ftm5 "abcde"]

testIsTextVarChar :: [Assertion]
testIsTextVarChar =
    let ftm5 a = fromTextMaybe a :: Maybe (VarChar 5)
    in [ Just (VarChar "") @=? ftm5 ""
       , Just (VarChar "abcde") @=? ftm5 "abcde"
       , Nothing @=? ftm5 "abcdef"]

testIsKey :: [Assertion]
testIsKey =
    let key :: Int -> Maybe Key
        key = fromTextMaybe . fromString . flip take (repeat '-')
    in
    [ Nothing @=? key 1
    , Nothing  @=? key 33
    , Just (Key $ fromString $ replicate 32 '-')  @=? key 32
    ]
