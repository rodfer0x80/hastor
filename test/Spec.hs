module Main where


import Test.Hspec

import BEncodeSpec
import MetaSpec


main :: IO ()
main = hspec $ do
  describe "BEncode" BEncodeSpec.spec
  describe "Meta" MetaSpec.spec
