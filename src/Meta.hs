{-# LANGUAGE OverloadedStrings #-}

module Meta (parseMeta) where

import BEncode (BEncode (..), encodeBEncode)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)

extractInteger :: BEncode -> Maybe Integer
extractInteger (BInt i) = Just i
extractInteger _ = Nothing

extractString :: BEncode -> Maybe ByteString
extractString (BString bs) = Just bs
extractString _ = Nothing

extractDict :: BEncode -> Maybe [(ByteString, BEncode)]
extractDict (BDict dict) = Just dict
extractDict _ = Nothing

lookupBEncode :: ByteString -> [(ByteString, BEncode)] -> Maybe BEncode
lookupBEncode key dict = lookup key dict

hashBDict :: BEncode -> String
hashBDict (BDict dict) = B.unpack $ Hex.encode $ SHA1.hash $ encodeBEncode (BDict dict)
hashBDict _ = ""

processPieceHashes :: ByteString -> String
processPieceHashes hashesBytes
  | B.null hashesBytes = ""
  | otherwise =
      let (currentHash, remainingHashes) = B.splitAt 20 hashesBytes
          hexHash = Hex.encode currentHash
       in "\n" ++ B.unpack hexHash ++ processPieceHashes remainingHashes

parseMeta :: BEncode -> String
parseMeta (BDict metadata) =
  let trackerUrlMaybe :: Maybe BEncode
      trackerUrlMaybe = lookupBEncode "announce" metadata
      trackerUrl :: Maybe ByteString
      trackerUrl = trackerUrlMaybe >>= extractString
      trackerStr = "Tracker URL: " ++ fromMaybe "N/A" (B.unpack <$> trackerUrl)

      infoDictMaybeBEncode :: Maybe BEncode
      infoDictMaybeBEncode = lookupBEncode "info" metadata
      infoDictMaybe :: Maybe [(ByteString, BEncode)]
      infoDictMaybe = infoDictMaybeBEncode >>= extractDict

      lengthStr = case infoDictMaybe of
        Just infoDict -> case lookupBEncode "length" infoDict >>= extractInteger of
          Just lenInt -> "Length: " ++ show lenInt
          Nothing -> "Length: N/A"
        Nothing -> "Length: N/A"

      infoHashStr = case infoDictMaybeBEncode of
        Just infoDictBEncode -> "Info Hash: " ++ hashBDict infoDictBEncode
        Nothing -> "Info Hash: N/A"

      pieceLengthStr = case infoDictMaybe of
        Just infoDict -> case lookupBEncode "piece length" infoDict >>= extractInteger of
          Just piecesLengthInt -> "Pieces Length: " ++ show piecesLengthInt
          Nothing -> "Pieces Length: N/A"
        Nothing -> "Pieces Length: N/A"

      pieceHashesStr = case infoDictMaybe of
        Just infoDict -> case lookupBEncode "pieces" infoDict >>= extractString of
          Just hashesBytes ->
            if B.null hashesBytes
              then "Piece Hashes: N/A"
              else "Piece Hashes: " ++ processPieceHashes hashesBytes -- unlines (map (B.unpack . encode) (chunksOf 20 hashesBytes))
          Nothing -> "Piece Hashes: N/A"
        Nothing -> "Piece Hashes: N/A"
   in trackerStr
        ++ "\n"
        ++ lengthStr
        ++ "\n"
        ++ infoHashStr
        ++ "\n"
        ++ pieceLengthStr
        ++ "\n"
        ++ pieceHashesStr
        ++ "\n"
parseMeta _ = "Error: Metadata is not a BEncoded dictionary\n"
