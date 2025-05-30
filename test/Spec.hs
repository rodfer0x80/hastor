module Main where


import Test.Hspec

import BEncodeSpec


main :: IO ()
main = hspec $ do
  describe "BEncode" BEncodeSpec.spec
