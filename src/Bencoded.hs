{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Bencoded where

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import Debug.Trace (trace)

data Bencoded
    = BInt Integer
    | BString ByteString
    | BList [Bencoded]
    deriving (Eq, Show)

instance ToJSON Bencoded where
    toJSON (BString string) = toJSON (B.unpack string)
    toJSON (BInt int) = toJSON int
    toJSON (BList xs) = toJSON (map toJSON xs)

decodeString :: ByteString -> (Bencoded, ByteString)
decodeString encodedValue =
    case B.elemIndex ':' encodedValue of
        Just colonIndex ->
            let (n, rest) = B.splitAt colonIndex encodedValue 
            in case B.readInt n of
                Just (stringLength, _) ->
                    let remainingAfterColon = B.drop 1 rest 
                        string = B.take (fromIntegral stringLength) remainingAfterColon 
                        remainChars = B.drop (fromIntegral stringLength) remainingAfterColon 
                    in if B.length string == fromIntegral stringLength 
                        then (BString string, remainChars)
                        else error "Invalid string length"
                Nothing -> error "Invalid string length"
        Nothing -> error "Invalid encoded value"

decodeInt :: ByteString -> (Bencoded, ByteString)
decodeInt encodedValue =
    case B.elemIndex 'e' encodedValue of
        Just eIndex ->
            let intPart = B.tail (B.take eIndex encodedValue)
            in case B.readInt intPart of
                Just (n, nonIntChars) | B.null nonIntChars -> (BInt (fromIntegral n), B.drop (eIndex + 1) encodedValue)
                _ -> error $ "Invalid integer encoding: " ++ B.unpack encodedValue
        Nothing -> error $ "Missing 'e' suffix for integer: " ++ B.unpack encodedValue

decodeListItems :: ByteString -> [Bencoded] -> (Bencoded, ByteString)
decodeListItems encodedValue acc = 
    case B.uncons encodedValue of
        Just ('e', remainChars) -> (BList (reverse acc), remainChars)
        Just (_, _) -> 
            let (decoded, remainChars) = decodeBencodedValue encodedValue 
            in decodeListItems remainChars (decoded : acc)
        Nothing -> error "Unterminated list"

decodeList :: ByteString -> (Bencoded, ByteString)
decodeList encodedValue = 
    if B.null encodedValue
        then error "Empty list encoding"
        else decodeListItems (B.tail encodedValue) []

decodeBencodedValue :: ByteString -> (Bencoded, ByteString)
decodeBencodedValue encodedValue
    | B.null encodedValue = error "Empty encoded value"
    | isDigit (B.head encodedValue) =
        decodeString encodedValue
    | B.head encodedValue == 'i' =
        decodeInt encodedValue
    | B.head encodedValue == 'l' =
        decodeList encodedValue
    | otherwise = 
        error $ "Unhandled encoded value: " ++ B.unpack encodedValue
