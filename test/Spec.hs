{-# LANGUAGE OverloadedStrings #-}


import Test.Hspec
import Test.Hspec.Expectations
import qualified Data.ByteString.Char8 as B
import Control.Exception (evaluate)

import Bencoded (Bencoded(..), decodeBencodedValue)


main :: IO ()
main = hspec $ do
  describe "decodeBencodedValue" $ do
    -- Integers
    it "should decode an integer" $ do
      decodeBencodedValue "i69e" `shouldBe` (BInt 69, B.empty)
    it "should decode a negative integer" $ do
      decodeBencodedValue "i-69e" `shouldBe` (BInt (-69), B.empty)
    
    -- Strings
    it "should decode a string" $ do
      decodeBencodedValue "5:hello" `shouldBe` (BString "hello", B.empty)
    it "should decode an empty string" $ do
      decodeBencodedValue "0:" `shouldBe` (BString "", B.empty)

    -- Lists
    it "should decode a simple list" $ do
      decodeBencodedValue "l5:helloi666e4:helle" `shouldBe` (BList [BString "hello", BInt 666, BString "hell"], B.empty)
    it "should decode a nested list" $ do
      decodeBencodedValue "ll3:blzi420eee" `shouldBe` (BList [BList [BString "blz", BInt 420]], B.empty)
    
    -- Error handling
    it "should handle remaining bytes" $ do
      decodeBencodedValue "i5eextraSAUCE" `shouldBe` (BInt 5, "extraSAUCE")
    
    -- Error exceptions
    it "should error on invalid integer encoding" $ do
      evaluate (decodeBencodedValue "i12a3e") `shouldThrow` anyException
    it "should error on missing 'e' for integer" $ do
      evaluate (decodeBencodedValue "i123") `shouldThrow` anyException
    it "should error on invalid string length" $ do
      evaluate (decodeBencodedValue "3:hi") `shouldThrow` anyException
    it "should error on unterminated list" $ do
      evaluate (decodeBencodedValue "l3:one") `shouldThrow` anyException

