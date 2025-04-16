{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module BEncodeSpec where


import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.ByteString.Char8 as B
import Control.Exception (evaluate)
import Data.Aeson
import Data.Map (fromList)

import BEncode (BEncode (..), decodeBEncode)


spec :: Spec
spec = do
  describe "decodeBEncode" $ do
    -- Integers
    describe "Integers" $ do
      it "should decode a positive integer" $
        decodeBEncode "i69e" `shouldBe` BInt 69
      it "should decode zero" $
        decodeBEncode "i0e" `shouldBe` BInt 0
      it "should decode a negative integer" $
        decodeBEncode "i-69e" `shouldBe` BInt (-69)
      it "should error on invalid integer encoding (non-digit)" $
        evaluate (decodeBEncode "i12a3e") `shouldThrow` anyException
      it "should error on missing 'e' for integer" $
        evaluate (decodeBEncode "i123") `shouldThrow` anyException
      it "should error on leading zero (positive)" $
        evaluate (decodeBEncode "i03e") `shouldThrow` anyException
      it "should error on leading zero (negative)" $
        evaluate (decodeBEncode "i-03e") `shouldThrow` anyException
      it "should error on negative zero" $
        evaluate (decodeBEncode "i-0e") `shouldThrow` anyException
      it "should decode the maximum positive integer" $
        decodeBEncode "i9223372036854775807e" `shouldBe` BInt 9223372036854775807
      it "should decode the minimum negative integer" $
        decodeBEncode "i-9223372036854775808e" `shouldBe` BInt (-9223372036854775808)
      it "should error on integer with leading zero after the sign" $
        evaluate (decodeBEncode "i-01e") `shouldThrow` anyException

    -- Strings
    describe "Strings" $ do
      it "should decode a simple string" $
        decodeBEncode "5:hello" `shouldBe` BString "hello"
      it "should decode an empty string" $
        decodeBEncode "0:" `shouldBe` BString ""
      it "should decode a string with special characters" $
        decodeBEncode "10:hello:!@#$" `shouldBe` BString "hello:!@#$"
      it "should error on invalid string length (too short)" $
        evaluate (decodeBEncode "3:hi") `shouldThrow` anyException
      it "should error on invalid string length (non-digit length)" $
        evaluate (decodeBEncode "a:test") `shouldThrow` anyException
      it "should error on missing colon" $
        evaluate (decodeBEncode "5hello") `shouldThrow` anyException
      -- TODO: fix this
      --it "should decode a string with multibyte UTF-8 characters" $
        --decodeBEncode "6:你好世界" `shouldBe` BString "你好世界"
      it "should decode a long string" $
        decodeBEncode (B.pack $ show (1000 :: Int) ++ ":" ++ replicate 1000 'a') `shouldBe` BString (B.pack $ replicate 1000 'a')
      it "should error if the length prefix is larger than the remaining bytestring" $
        evaluate (decodeBEncode "10:short") `shouldThrow` anyException

    -- Lists
    describe "Lists" $ do
      it "should decode an empty list" $
        decodeBEncode "le" `shouldBe` BList []
      it "should decode a simple list of strings" $
        decodeBEncode "l3:one3:two5:threee" `shouldBe` BList [BString "one", BString "two", BString "three"]
      it "should decode a list of integers" $
        decodeBEncode "li1ei2ei3ee" `shouldBe` BList [BInt 1, BInt 2, BInt 3]
      it "should decode a mixed list" $
        decodeBEncode "l5:helloi666ee" `shouldBe` BList [BString "hello", BInt 666]
      it "should decode a nested list" $
        decodeBEncode "ll3:blzi420eee" `shouldBe` BList [BList [BString "blz", BInt 420]]
      it "should error on unterminated list" $
        evaluate (decodeBEncode "l3:one") `shouldThrow` anyException
      it "should error on empty list start but no end" $
        evaluate (decodeBEncode "l") `shouldThrow` anyException
      it "should decode a list with an empty string" $
        decodeBEncode "l0:ee" `shouldBe` BList [BString ""]
      it "should decode a list with an empty list" $
        decodeBEncode "llee" `shouldBe` BList [BList []]
      it "should decode a list with an empty dictionary" $
        decodeBEncode "ldeee" `shouldBe` BList [BDict []]

    -- Dictionaries
    describe "Dictionaries" $ do
      it "should decode an empty dictionary" $
        decodeBEncode "de" `shouldBe` BDict []
      it "should decode a simple dictionary (string keys)" $
        decodeBEncode "d3:foo3:bar5:helloi42ee" `shouldBe` BDict [("foo", BString "bar"), ("hello", BInt 42)]
      it "should decode a dictionary with integer values" $
        decodeBEncode "d3:onei1e3:twoi2ee" `shouldBe` BDict [("one", BInt 1), ("two", BInt 2)]
      it "should decode a dictionary with mixed values" $
        decodeBEncode "d3:onei1e3:two4:fivee" `shouldBe` BDict [("one", BInt 1), ("two", BString "five")]
      it "should decode a nested dictionary" $
        decodeBEncode "d4:jumpd2:upi13e4:downi666eee" `shouldBe` BDict [("jump", BDict [("up", BInt 13), ("down", BInt 666)])]
      it "should error on unterminated dictionary" $
        evaluate (decodeBEncode "d3:foo3:bar") `shouldThrow` anyException
      it "should error on odd number of elements in dictionary" $
        evaluate (decodeBEncode "d3:fooe") `shouldThrow` anyException
      it "should error if key is not a string" $
        evaluate (decodeBEncode "di1ei2ee") `shouldThrow` anyException
      it "should handle empty dictionary start but no end" $
        evaluate (decodeBEncode "d") `shouldThrow` anyException
      it "should decode a dictionary with an empty string key" $
        decodeBEncode "d0:i0ee" `shouldBe` BDict [("", BInt 0)]
      it "should decode a dictionary with an empty string value" $
        decodeBEncode "d3:foo0:ee" `shouldBe` BDict [("foo", BString "")]
      -- TODO: fix this
      --it "should decode a dictionary with an empty list value" $
        --decodeBEncode "d3:foollee" `shouldBe` BDict [("foo", BList [])]
      it "should decode a dictionary with an empty dictionary value" $
        decodeBEncode "d3:foodeee" `shouldBe` BDict [("foo", BDict [])]
      it "should handle multiple key-value pairs" $
        decodeBEncode "d1:ai1e1:bi2ee" `shouldBe` BDict [("a", BInt 1), ("b", BInt 2)]

    -- Error handling for empty input
    it "should error on empty input" $
      evaluate (decodeBEncode B.empty) `shouldThrow` anyException

    -- Error handling for unexpected characters at the beginning
    describe "Unexpected Characters at Start" $ do
      it "should error on unexpected character before integer" $
        evaluate (decodeBEncode "ai1ee") `shouldThrow` anyException
      it "should error on unexpected character before string" $
        evaluate (decodeBEncode "a5:hello") `shouldThrow` anyException
      it "should error on unexpected character before list" $
        evaluate (decodeBEncode "ale") `shouldThrow` anyException
      it "should error on unexpected character before dictionary" $
        evaluate (decodeBEncode "ade") `shouldThrow` anyException

    -- Error handling for truncated input
    describe "Truncated Input" $ do
      it "should error on truncated integer" $
        evaluate (decodeBEncode "i12") `shouldThrow` anyException
      it "should error on truncated string length" $
        evaluate (decodeBEncode "5") `shouldThrow` anyException
      it "should error on truncated string content" $
        evaluate (decodeBEncode "5:hell") `shouldThrow` anyException
      it "should error on truncated list start" $
        evaluate (decodeBEncode "l") `shouldThrow` anyException
      it "should error on truncated dictionary start" $
        evaluate (decodeBEncode "d") `shouldThrow` anyException
      it "should error on truncated dictionary key length" $
        evaluate (decodeBEncode "d3") `shouldThrow` anyException
      it "should error on truncated dictionary key content" $
        evaluate (decodeBEncode "d3:fo") `shouldThrow` anyException
      it "should error on truncated dictionary value" $
        evaluate (decodeBEncode "d3:fooi1") `shouldThrow` anyException
