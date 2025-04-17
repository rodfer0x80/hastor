{-# LANGUAGE OverloadedStrings #-}

module Meta (parseMeta) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString.Base16 (encode)
import Data.Maybe (fromMaybe)
import BEncode (BEncode (..), encodeBEncode)

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
hashBDict dict = B.unpack $ encode $ SHA1.hash $ encodeBEncode dict

parseMeta :: BEncode -> String
parseMeta (BDict metadata) =
    let
        trackerUrlMaybe :: Maybe BEncode
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
            Nothing -> "Error: Info dictionary not found"
        
        infoHashStr = case infoDictMaybeBEncode of
            Just infoDict -> "Info Hash: " ++ hashBDict infoDict
            Nothing -> "Error: Info dictionary not found"
    in
        trackerStr ++ "\n" ++ lengthStr ++ "\n" ++ infoHashStr ++ "\n"
parseMeta _ = "Error: Metadata is not a BEncoded dictionary\n"
