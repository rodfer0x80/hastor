module MetaSpec where

import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import BEncode (BEncode (..))
import Meta (parseMeta)

spec :: Spec
spec = do
  describe "parseMeta" $ do
    it "should correctly parse a valid metadata dictionary" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
              (B.pack "info", BDict [
                  (B.pack "length", BInt 12345)
                ])
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 35a51203445323b77f936eed3c522b56f7cfc2e1\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'announce' key" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "info", BDict [
                  (B.pack "length", BInt 12345)
                ])
            ]
          expectedOutput = "Tracker URL: N/A\nLength: 12345\nInfo Hash: 35a51203445323b77f936eed3c522b56f7cfc2e1\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'info' key" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker")
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'length' key in the 'info' dictionary" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
              (B.pack "info", BDict [])
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: 600ccd1b71569232d01d110bc63e906beab04d8c\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle non-integer value for 'length'" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
              (B.pack "info", BDict [
                  (B.pack "length", BString $ B.pack "not an integer")
                ])
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: 0608cb32c9810e190a868ae06eb926dede06a36a\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should return an error message for non-dictionary input" $ do
      let metadata :: BEncode
          metadata = BString $ B.pack "not a dictionary"
          expectedOutput = "Error: Metadata is not a BEncoded dictionary\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle an empty dictionary" $ do
      let metadata :: BEncode
          metadata = BDict []
          expectedOutput = "Tracker URL: N/A\nLength: N/A\nInfo Hash: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a dictionary with extra keys" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
              (B.pack "info", BDict [
                  (B.pack "length", BInt 12345),
                  (B.pack "name", BString $ B.pack "example.txt")
                ]),
              (B.pack "creation date", BInt 1681728000)
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 4403baf75248fb567f7977c3dd7ee9a257067eef\n"
      parseMeta metadata `shouldBe` expectedOutput
