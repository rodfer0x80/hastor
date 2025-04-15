{-# LANGUAGE OverloadedStrings #-}


module Metainfo (parseMetainfo) where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)

import BEncode (BEncode (..))


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


parseMetainfo :: BEncode -> String
parseMetainfo (BDict topLevel) =
    let
        trackerUrlMaybe :: Maybe BEncode
        trackerUrlMaybe = lookupBEncode "announce" topLevel
        trackerUrl :: Maybe ByteString
        trackerUrl = trackerUrlMaybe >>= extractString
        trackerStr = "Tracker URL: " ++ fromMaybe "N/A" (B.unpack <$> trackerUrl)

        infoDictMaybeBEncode :: Maybe BEncode
        infoDictMaybeBEncode = lookupBEncode "info" topLevel
        infoDictMaybe :: Maybe [(ByteString, BEncode)]
        infoDictMaybe = infoDictMaybeBEncode >>= extractDict
        lengthStr = case infoDictMaybe of
            Just infoDict -> case lookupBEncode "length" infoDict >>= extractString of
                Just lenStr -> "Length: " ++ B.unpack lenStr
                Nothing -> case lookupBEncode "length" infoDict >>= extractInteger of
                    Just lenInt -> "Length: " ++ show lenInt
                    Nothing -> "Length: N/A"
            Nothing -> "Info dictionary not found"
    in
        trackerStr ++ "\n" ++ lengthStr ++ "\n"
parseMetainfo _ = "Error: Top-level is not a dictionary\n"
