{-# LANGUAGE OverloadedStrings #-}

module Meta (parseMeta, getPeers, requestPeers) where

import BEncode (BEncode (..), decodeBEncode, encodeBEncode)
import Utils(extractInteger, extractByteString, lookupBEncode, processPieceHashes, parseTrackerUrl, parseInfoHash, parseInfoDict, parseinfoHashUrlEncoded)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base16 as Hex
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
import Network.URI.Encode as URL

getPeers :: BEncode -> String
getPeers (BDict metadata) =
  let trackerUrl = fromMaybe "" (parseTrackerUrl (BDict metadata))
      urlEncodedHashMaybe = parseinfoHashUrlEncoded (BDict metadata)
      peerId = "13374204204204201337"
      port = "6881"
      uploaded = "0"
      downloaded = "0"
      left = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "length" infoDict >>= extractInteger of
          Just lenInt -> show lenInt
          Nothing -> "0"
        Nothing -> "0"
      compact = "1"
   in case urlEncodedHashMaybe of
        Just urlEncodedHash ->
          "tracker_url: "
            ++ trackerUrl
            ++ "\n"
            ++ "info_hash: "
            ++ urlEncodedHash
            ++ "\n"
            ++ "peer_id: "
            ++ peerId
            ++ "\n"
            ++ "port: "
            ++ port
            ++ "\n"
            ++ "uploaded: "
            ++ uploaded
            ++ "\n"
            ++ "downloaded: "
            ++ downloaded
            ++ "\n"
            ++ "left: "
            ++ left
            ++ "\n"
            ++ "compact: "
            ++ compact
            ++ "\n"
        Nothing -> "Error: Could not extract or encode info_hash\n"

requestPeers :: BEncode -> IO String
requestPeers (BDict metadata) = do
  let trackerUrl = fromMaybe "" (parseTrackerUrl (BDict metadata))
      urlEncodedHashMaybe = parseinfoHashUrlEncoded (BDict metadata)
      peerId = "13374204204204201337"
      port = "6881"
      uploaded = "0"
      downloaded = "0"
      left = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "length" infoDict >>= extractInteger of
          Just lenInt -> show lenInt
          Nothing -> "0"
        Nothing -> "0"
      compact = "1"
  case urlEncodedHashMaybe of
    Just urlEncodedHash -> do
      let queryString =
            "?info_hash="
              ++ urlEncodedHash
              ++ "&peer_id="
              ++ peerId
              ++ "&port="
              ++ port
              ++ "&uploaded="
              ++ uploaded
              ++ "&downloaded="
              ++ downloaded
              ++ "&left="
              ++ left
              ++ "&compact="
              ++ compact
          fullUrl = trackerUrl ++ queryString
      let request = parseRequest_ fullUrl
      response <- httpLBS request
      let statusCode = getResponseStatusCode response
          lazyBody = getResponseBody response
          decodedBody = decodeBEncode $ L.toStrict lazyBody
      return ("Status Code: " ++ show statusCode ++ "\nBody (Decoded):\n" ++ show decodedBody ++ "\n")
    Nothing -> return "Error: Could not generate URL for tracker request (invalid info_hash).\n"

parseMeta :: BEncode -> String
parseMeta (BDict metadata) =
  let trackerStr = "Tracker URL: " ++ fromMaybe "N/A" (parseTrackerUrl (BDict metadata))

      lengthStr = case parseInfoDict (BDict metadata) of
        Just infoDict -> case lookupBEncode "length" infoDict >>= extractInteger of
          Just lenInt -> "Length: " ++ show lenInt
          Nothing -> "Length: N/A"
        Nothing -> "Length: N/A"

      infoHashStr = case parseInfoHash (BDict metadata) of
        Just hashBytes -> "Info Hash: " ++ (B.unpack (Hex.encode hashBytes)) 
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
