{-# LANGUAGE OverloadedStrings #-}

module Meta (parseMeta, getPeers) where

import BEncode (BEncode (..), encodeBEncode)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Network.URI.Encode as B64

extractInteger :: BEncode -> Maybe Integer
extractInteger (BInt i) = Just i
extractInteger _ = Nothing

extractString :: BEncode -> Maybe String
extractString (BString bs) = Just $ B.unpack bs
extractString _ = Nothing

extractByteString :: BEncode -> Maybe ByteString
extractByteString (BString bs) = Just bs
extractByteString _ = Nothing

extractDict :: BEncode -> Maybe [(ByteString, BEncode)]
extractDict (BDict dict) = Just dict
extractDict _ = Nothing

lookupBEncode :: ByteString -> [(ByteString, BEncode)] -> Maybe BEncode
lookupBEncode key dict = lookup key dict

hashBDict :: BEncode -> ByteString
hashBDict (BDict dict) = SHA1.hash $ encodeBEncode (BDict dict)
hashBDict _ = ""

processPieceHashes :: ByteString -> String
processPieceHashes hashesBytes
  | B.null hashesBytes = ""
  | otherwise =
      let (currentHash, remainingHashes) = B.splitAt 20 hashesBytes
          hexHash = Hex.encode currentHash
       in "\n" ++ B.unpack hexHash ++ processPieceHashes remainingHashes

parseTrackerUrl :: BEncode -> Maybe String
parseTrackerUrl (BDict metadata) =
  let trackerUrlMaybe :: Maybe BEncode
      trackerUrlMaybe = lookupBEncode "announce" metadata
      trackerUrl :: Maybe String
      trackerUrl = trackerUrlMaybe >>= extractString
   in trackerUrl

parseInfoDict :: BEncode -> Maybe [(ByteString, BEncode)]
parseInfoDict (BDict metadata) =
  let infoDictMaybeBEncode :: Maybe BEncode
      infoDictMaybeBEncode = lookupBEncode "info" metadata
      infoDictMaybe :: Maybe [(ByteString, BEncode)]
      infoDictMaybe = infoDictMaybeBEncode >>= extractDict
   in infoDictMaybe

parseInfoHash :: BEncode -> String
parseInfoHash (BDict metadata) = B.unpack $ Hex.encode $ hashBDict (BDict metadata)

parseinfoHashUrlEncoded :: BEncode -> String
parseinfoHashUrlEncoded (BDict metadata) = B.unpack $ B64.encodeByteString $ hashBDict (BDict metadata)

getPeers :: BEncode -> String
getPeers (BDict metadata) =
  let trackerUrl = fromMaybe "" (parseTrackerUrl (BDict metadata))
      urlEncodedHash = parseinfoHashUrlEncoded (BDict metadata)
      lengthStr = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "length" infoDict >>= extractInteger of
          Just lenInt -> show lenInt
          Nothing -> "0"
        Nothing -> "0"
   in "tracker_url: " ++ trackerUrl ++ "\n" ++ "info_hash: " ++ urlEncodedHash ++ "\n" ++ "peer_id: " ++ "13374204204204201337" ++ "\n" ++ "ports: " ++ "6881" ++ "\n" ++ "uploaded: " ++ "0" ++ "\n" ++ "downloaded: " ++ "0" ++ "\n" ++ "left: " ++ lengthStr ++ "\n" ++ "compact: " ++ "1" ++ "\n"

parseMeta :: BEncode -> String
parseMeta (BDict metadata) =
  let trackerStr = "Tracker URL: " ++ fromMaybe "N/A" (parseTrackerUrl (BDict metadata))

      lengthStr = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "length" infoDict >>= extractInteger of
          Just lenInt -> "Length: " ++ show lenInt
          Nothing -> "Length: N/A"
        Nothing -> "Length: N/A"

      infoHashStr = case (lookupBEncode "info" metadata) of
        Just infoDictBEncode -> "Info Hash: " ++ (B.unpack (hashBDict infoDictBEncode))
        Nothing -> "Info Hash: N/A"

      pieceLengthStr = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "piece length" infoDict >>= extractInteger of
          Just piecesLengthInt -> "Pieces Length: " ++ show piecesLengthInt
          Nothing -> "Pieces Length: N/A"
        Nothing -> "Pieces Length: N/A"

      pieceHashesStr = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "pieces" infoDict >>= extractByteString of
          Just hashesBytes ->
            if B.null hashesBytes
              then "Piece Hashes: N/A"
              else "Piece Hashes: " ++ processPieceHashes hashesBytes
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
