{-# LANGUAGE OverloadedStrings #-}

module Utils (extractString, extractInteger, extractByteString, extractDict, lookupBEncode, hashBDict, processPieceHashes, parseTrackerUrl, parseInfoHash, parseInfoDict, parseinfoHashUrlEncoded) where

import BEncode (BEncode (..), decodeBEncode, encodeBEncode)

import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Network.URI.Encode as URL

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

parseInfoHash :: BEncode -> Maybe ByteString
parseInfoHash (BDict metadata) = do
  infoDictBEncode <- lookupBEncode "info" metadata
  return $ hashBDict infoDictBEncode

parseinfoHashUrlEncoded :: BEncode -> Maybe String
parseinfoHashUrlEncoded (BDict metadata) = do
  infoHash <- parseInfoHash (BDict metadata)
  return $ B.unpack $ URL.encodeByteString infoHash
