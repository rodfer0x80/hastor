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
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 6b86b273ff34fce19d6b804eff5a3f5747ada4aa\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'announce' key" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "info", BDict [
                  (B.pack "length", BInt 12345)
                ])
            ]
          expectedOutput = "Tracker URL: N/A\nLength: 12345\nInfo Hash: 6b86b273ff34fce19d6b804eff5a3f5747ada4aa\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'info' key" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker")
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nError: Info dictionary not found\nError: Info dictionary not found\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'length' key in the 'info' dictionary" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
              (B.pack "info", BDict [])
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: da39a3ee5e6b4b0d3255bfef95601890afd80709\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle non-integer value for 'length'" $ do
      let metadata :: BEncode
          metadata = BDict [
              (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
              (B.pack "info", BDict [
                  (B.pack "length", BString $ B.pack "not an integer")
                ])
            ]
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: 6b86b273ff34fce19d6b804eff5a3f5747ada4aa\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should return an error message for non-dictionary input" $ do
      let metadata :: BEncode
          metadata = BString $ B.pack "not a dictionary"
          expectedOutput = "Error: Metadata is not a BEncoded dictionary\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle an empty dictionary" $ do
      let metadata :: BEncode
          metadata = BDict []
          expectedOutput = "Tracker URL: N/A\nError: Info dictionary not found\nError: Info dictionary not found\n"
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
          expectedOutput = "Tracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 6b86b273ff34fce19d6b804eff5a3f5747ada4aa\n"
      parseMeta metadata `shouldBe` expectedOutput
