module MetaSpec where

import BEncode (BEncode (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Meta (parseMeta)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMeta" $ do
    it "should correctly parse a valid metadata dictionary with pieces info" $ do
      let metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
                ( B.pack "info",
                  BDict
                    [ (B.pack "length", BInt 12345),
                      (B.pack "piece length", BInt 8192),
                      (B.pack "pieces", BString $ B.pack "somepiecehashes")
                    ]
                )
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 16e4e76803e108ff79a979b63aa7fa65e5151b25\nPieces Length: 8192\nPiece Hashes: \n736f6d657069656365686173686573\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'announce' key" $ do
      let metadata :: BEncode
          metadata =
            BDict
              [ ( B.pack "info",
                  BDict
                    [ (B.pack "length", BInt 12345),
                      (B.pack "piece length", BInt 8192),
                      (B.pack "pieces", BString $ B.pack "somepiecehashes")
                    ]
                )
              ]
          expectedOutput = "\nTracker URL: N/A\nLength: 12345\nInfo Hash: 16e4e76803e108ff79a979b63aa7fa65e5151b25\nPieces Length: 8192\nPiece Hashes: \n736f6d657069656365686173686573\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'info' key" $ do
      let metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker")
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: N/A\nPieces Length: N/A\nPiece Hashes: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle missing 'length', 'piece length', and 'pieces' keys in the 'info' dictionary" $ do
      let infoDict = BDict []
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: 600ccd1b71569232d01d110bc63e906beab04d8c\nPieces Length: N/A\nPiece Hashes: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle non-integer value for 'length'" $ do
      let infoDict = BDict
                        [ (B.pack "length", BString $ B.pack "not an integer"),
                          (B.pack "piece length", BInt 8192),
                          (B.pack "pieces", BString $ B.pack "somepiecehashes")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: N/A\nInfo Hash: 6a5f9f577d8d30241b973b99fad310baca558112\nPieces Length: 8192\nPiece Hashes: \n736f6d657069656365686173686573\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle non-integer value for 'piece length'" $ do
      let infoDict = BDict
                        [ (B.pack "length", BInt 12345),
                          (B.pack "piece length", BString $ B.pack "not an integer"),
                          (B.pack "pieces", BString $ B.pack "somepiecehashes")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 321fec476ff95e3fd831e0585581f603d937d608\nPieces Length: N/A\nPiece Hashes: \n736f6d657069656365686173686573\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle non-string value for 'pieces'" $ do
      let infoDict = BDict
                        [ (B.pack "length", BInt 12345),
                          (B.pack "piece length", BInt 8192),
                          (B.pack "pieces", BInt 123)
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: 60b79a98205e2c85912401ed0db53b2c469a0e23\nPieces Length: 8192\nPiece Hashes: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should return an error message for non-dictionary input" $ do
      let metadata :: BEncode
          metadata = BString $ B.pack "not a dictionary"
          expectedOutput = "Error: Metadata is not a BEncoded dictionary\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle an empty dictionary" $ do
      let metadata :: BEncode
          metadata = BDict []
          expectedOutput = "\nTracker URL: N/A\nLength: N/A\nInfo Hash: N/A\nPieces Length: N/A\nPiece Hashes: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a dictionary with extra keys" $ do
      let infoDict = BDict
                        [ (B.pack "length", BInt 12345),
                          (B.pack "piece length", BInt 8192),
                          (B.pack "pieces", BString $ B.pack "somepiecehashes"),
                          (B.pack "name", BString $ B.pack "example.txt")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://example.com/tracker"),
                (B.pack "info", infoDict),
                (B.pack "creation date", BInt 1681728000)
              ]
          expectedOutput = "\nTracker URL: http://example.com/tracker\nLength: 12345\nInfo Hash: e234a3c12c386539870b88f692393d6d77db42e8\nPieces Length: 8192\nPiece Hashes: \n736f6d657069656365686173686573\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a metadata dictionary without 'piece length' and 'pieces'" $ do
      let infoDict = BDict
                        [ (B.pack "length", BInt 54321)
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://othertracker.com/announce"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://othertracker.com/announce\nLength: 54321\nInfo Hash: 8deb07211c507a70ea115ff08dee8e09e39627f4\nPieces Length: N/A\nPiece Hashes: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a metadata dictionary with an empty 'pieces' string" $ do
      let infoDict = BDict
                        [ (B.pack "length", BInt 9876),
                          (B.pack "piece length", BInt 4096),
                          (B.pack "pieces", BString $ B.pack "")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "udp://tracker.local:8080/announce"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: udp://tracker.local:8080/announce\nLength: 9876\nInfo Hash: b1217567d2c28954b12994b1129e801987f01df0\nPieces Length: 4096\nPiece Hashes: N/A\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a metadata dictionary with multiple files (no 'length' at the top level of 'info')" $ do
      let infoDict = BDict
                        [ (B.pack "files", BList [
                            BDict [ (B.pack "length", BInt 1000), (B.pack "path", BList [BString $ B.pack "file1.txt"]) ],
                            BDict [ (B.pack "length", BInt 2000), (B.pack "path", BList [BString $ B.pack "dir", BString $ B.pack "file2.bin"]) ]
                          ]),
                          (B.pack "piece length", BInt 8192),
                          (B.pack "pieces", BString $ B.pack "anotherhash")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://multitracker.com/announce"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://multitracker.com/announce\nLength: N/A\nInfo Hash: c8d9e509c92de76254947ad35e09e61aa42b294f\nPieces Length: 8192\nPiece Hashes: \n616e6f7468657268617368\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a metadata dictionary with a missing 'files' key in multi-file mode" $ do
      let infoDict = BDict
                        [ (B.pack "piece length", BInt 4096),
                          (B.pack "pieces", BString $ B.pack "someotherhash")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://yetanothertracker.com/announce"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://yetanothertracker.com/announce\nLength: N/A\nInfo Hash: a20ce83f366051002506b32f8b1aab5c80818a03\nPieces Length: 4096\nPiece Hashes: \n736f6d656f7468657268617368\n"
      parseMeta metadata `shouldBe` expectedOutput

    it "should handle a metadata dictionary with an empty 'files' list" $ do
      let infoDict = BDict
                        [ (B.pack "files", BList []),
                          (B.pack "piece length", BInt 8192),
                          (B.pack "pieces", BString $ B.pack "lastpiecehash")
                        ]
          metadata :: BEncode
          metadata =
            BDict
              [ (B.pack "announce", BString $ B.pack "http://finaltracker.net/announce"),
                (B.pack "info", infoDict)
              ]
          expectedOutput = "\nTracker URL: http://finaltracker.net/announce\nLength: N/A\nInfo Hash: cce936ca9dc8956549198e8f0e4be4b5f7cc8d87\nPieces Length: 8192\nPiece Hashes: \n6c617374706965636568617368\n"
      parseMeta metadata `shouldBe` expectedOutput
