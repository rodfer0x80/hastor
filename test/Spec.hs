{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.ByteString.Char8 as B
import Control.Exception (evaluate)

import BEncode (BEncode (..), decodeBEncode)

main :: IO ()
main = hspec $ do
  describe "decodeBEncode" $ do
    -- Integers
    describe "Integers" $ do
      it "should decode a positive integer" $ do
        decodeBEncode "i69e" `shouldBe` (BInt 69, B.empty)
      it "should decode zero" $ do
        decodeBEncode "i0e" `shouldBe` (BInt 0, B.empty)
      it "should decode a negative integer" $ do
        decodeBEncode "i-69e" `shouldBe` (BInt (-69), B.empty)
      it "should handle remaining bytes after integer" $ do
        decodeBEncode "i5eextraSAUCE" `shouldBe` (BInt 5, "extraSAUCE")
      it "should error on invalid integer encoding (non-digit)" $ do
        evaluate (decodeBEncode "i12a3e") `shouldThrow` anyException
      it "should error on missing 'e' for integer" $ do
        evaluate (decodeBEncode "i123") `shouldThrow` anyException
      it "should error on leading zero (positive)" $ do
        evaluate (decodeBEncode "i03e") `shouldThrow` anyException
      it "should error on leading zero (negative)" $ do
        evaluate (decodeBEncode "i-03e") `shouldThrow` anyException
      it "should error on negative zero" $ do
        evaluate (decodeBEncode "i-0e") `shouldThrow` anyException

    -- Strings
    describe "Strings" $ do
      it "should decode a simple string" $ do
        decodeBEncode "5:hello" `shouldBe` (BString "hello", B.empty)
      it "should decode an empty string" $ do
        decodeBEncode "0:" `shouldBe` (BString "", B.empty)
      it "should decode a string with special characters" $ do
        decodeBEncode "10:hello:!@#$" `shouldBe` (BString "hello:!@#$", B.empty)
      it "should handle remaining bytes after string" $ do
        decodeBEncode "5:worldMORE" `shouldBe` (BString "world", "MORE")
      it "should error on invalid string length (too short)" $ do
        evaluate (decodeBEncode "3:hi") `shouldThrow` anyException
      it "should error on invalid string length (non-digit length)" $ do
        evaluate (decodeBEncode "a:test") `shouldThrow` anyException
      it "should error on missing colon" $ do
        evaluate (decodeBEncode "5hello") `shouldThrow` anyException

    -- Lists
    describe "Lists" $ do
      it "should decode an empty list" $ do
        decodeBEncode "le" `shouldBe` (BList [], B.empty)
      it "should decode a simple list of strings" $ do
        decodeBEncode "l3:one3:two5:threee" `shouldBe` (BList [BString "one", BString "two", BString "three"], B.empty)
      it "should decode a list of integers" $ do
        decodeBEncode "li1ei2ei3ee" `shouldBe` (BList [BInt 1, BInt 2, BInt 3], B.empty)
      it "should decode a mixed list" $ do
        decodeBEncode "l5:helloi666ee" `shouldBe` (BList [BString "hello", BInt 666], B.empty)
      it "should decode a nested list" $ do
        decodeBEncode "ll3:blzi420eee" `shouldBe` (BList [BList [BString "blz", BInt 420]], B.empty)
      it "should handle remaining bytes after list" $ do
        decodeBEncode "leEXTRA" `shouldBe` (BList [], "EXTRA")
      it "should error on unterminated list" $ do
        evaluate (decodeBEncode "l3:one") `shouldThrow` anyException
      it "should error on empty list start but no end" $ do
        evaluate (decodeBEncode "l") `shouldThrow` anyException

    -- Dictionaries
    describe "Dictionaries" $ do
      it "should decode an empty dictionary" $ do
        decodeBEncode "de" `shouldBe` (BDict [], B.empty)
      it "should decode a simple dictionary (string keys)" $ do
        decodeBEncode "d3:foo3:bar5:helloi42ee" `shouldBe` (BDict [(B.pack "foo", BString (B.pack "bar")), (B.pack "hello", BInt 42)], B.empty)
      it "should decode a dictionary with integer values" $ do
        decodeBEncode "d3:onei1e3:twoi2ee" `shouldBe` (BDict [(B.pack "one", BInt 1), (B.pack "two", BInt 2)], B.empty)
      it "should decode a dictionary with mixed values" $ do
        decodeBEncode "d3:onei1e3:two4:fivee" `shouldBe` (BDict [(B.pack "one", BInt 1), (B.pack "two", BString (B.pack "five"))], B.empty)
      it "should decode a nested dictionary" $ do
        decodeBEncode "d4:jumpd2:upi13e4:downi666eee" `shouldBe` (BDict [(B.pack "jump", BDict [(B.pack "up", BInt 13), (B.pack "down", BInt 666)])], B.empty)
      it "should handle remaining bytes after dictionary" $ do
        decodeBEncode "deJUNK" `shouldBe` (BDict [], "JUNK")
      it "should error on unterminated dictionary" $ do
        evaluate (decodeBEncode "d3:foo3:bar") `shouldThrow` anyException
      it "should error on odd number of elements in dictionary" $ do
        evaluate (decodeBEncode "d3:fooe") `shouldThrow` anyException
      it "should error if key is not a string" $ do
        evaluate (decodeBEncode "di1ei2ee") `shouldThrow` anyException
      it "should handle empty dictionary start but no end" $ do
        evaluate (decodeBEncode "d") `shouldThrow` anyException

    -- Error handling for empty input
    it "should error on empty input" $ do
      evaluate (decodeBEncode B.empty) `shouldThrow` anyException
