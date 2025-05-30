{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BEncode where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Key (Key, fromText)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List (sortBy)
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace (trace)

data BEncode
  = BInt Integer
  | BString ByteString
  | BList [BEncode]
  | BDict [(ByteString, BEncode)]
  deriving (Eq, Show)

instance ToJSON BEncode where
  toJSON (BString string) = toJSON (B.unpack string)
  toJSON (BInt int) = toJSON int
  toJSON (BList xs) = toJSON (map toJSON xs)
  toJSON (BDict dict) = object $ map (\(k, v) -> (fromStrict k) .= v) dict
    where
      fromStrict :: ByteString -> Key
      fromStrict bs = fromText (decodeUtf8 bs)

parsePrefix :: (ByteString -> Maybe (a, ByteString)) -> ByteString -> Maybe (a, ByteString)
parsePrefix p bs = p bs

parseChar :: Char -> ByteString -> Maybe (Char, ByteString)
parseChar c bs
  | not (B.null bs) && B.head bs == c = Just (c, B.tail bs)
  | otherwise = Nothing

parseIntegerBS :: ByteString -> Maybe (Integer, ByteString)
parseIntegerBS bs
  | B.null bs = Nothing
  | otherwise = case B.readInt bs of
      Just (n, rest) | B.all isDigit (B.take (B.length bs - B.length rest) bs) -> Just (fromIntegral n, rest)
      _ -> Nothing

decodeString :: ByteString -> Maybe (BEncode, ByteString)
decodeString bs = do
  (len, rest1) <- parsePrefix parseIntegerBS bs
  (_colon, rest2) <- parsePrefix (parseChar ':') rest1
  let string = B.take (fromIntegral len) rest2
      remaining = B.drop (fromIntegral len) rest2
  guard (B.length string == fromIntegral len)
  return (BString string, remaining)

decodeInt :: ByteString -> Maybe (BEncode, ByteString)
decodeInt bs = do
  (_i, rest1) <- parsePrefix (parseChar 'i') bs
  (intPartBS, rest2) <- parseWhileNot 'e' rest1
  (_e, rest3) <- parsePrefix (parseChar 'e') rest2
  let encodedIntStr = B.unpack intPartBS
  n <- case B.readInt intPartBS of
    Just (val, r) | B.null r -> Just (fromIntegral val)
    _ -> Nothing
  guard $ not ((not (null encodedIntStr) && head encodedIntStr == '0' && length encodedIntStr > 1) || encodedIntStr == "-0" || (length encodedIntStr > 1 && head encodedIntStr == '-' && encodedIntStr !! 1 == '0'))
  return (BInt n, rest3)
  where
    parseWhileNot c s = case B.uncons s of
      Just (h, t) | h /= c -> fmap (\(res, rem) -> (B.cons h res, rem)) (parseWhileNot c t)
      _ -> Just (B.empty, s)

decodeList :: ByteString -> Maybe (BEncode, ByteString)
decodeList bs = do
  (_l, rest) <- parsePrefix (parseChar 'l') bs
  (items, remaining) <- foldWhileJust decodeBEncode' [] rest
  (_e, finalRemaining) <- parsePrefix (parseChar 'e') remaining
  return (BList (reverse items), finalRemaining)
  where
    foldWhileJust :: (ByteString -> Maybe (a, ByteString)) -> [a] -> ByteString -> Maybe ([a], ByteString)
    foldWhileJust f acc s = case f s of
      Just (val, rest) -> foldWhileJust f (val : acc) rest
      Nothing -> Just (acc, s)

decodeDictionary :: ByteString -> Maybe (BEncode, ByteString)
decodeDictionary bs = do
  (_d, rest) <- parsePrefix (parseChar 'd') bs
  (items, remaining) <- foldWhileJust decodePair [] rest
  (_e, finalRemaining) <- parsePrefix (parseChar 'e') remaining
  return (BDict (reverse items), finalRemaining)
  where
    decodePair s = do
      (keyDecoded, rest1) <- decodeString s
      case keyDecoded of
        BString key -> do
          (valueDecoded, rest2) <- decodeBEncode' rest1
          return ((key, valueDecoded), rest2)
        _ -> Nothing
    foldWhileJust :: (ByteString -> Maybe (a, ByteString)) -> [a] -> ByteString -> Maybe ([a], ByteString)
    foldWhileJust f acc s = case f s of
      Just (val, rest) -> foldWhileJust f (val : acc) rest
      Nothing -> Just (acc, s)

decodeBEncode' :: ByteString -> Maybe (BEncode, ByteString)
decodeBEncode' bs
  | B.null bs = Nothing
  | isDigit (B.head bs) = decodeString bs
  | B.head bs == 'i' = decodeInt bs
  | B.head bs == 'l' = decodeList bs
  | B.head bs == 'd' = decodeDictionary bs
  | otherwise = Nothing

-- TODO: should error/handling be done in Main and Spec according to needs instead of generally?
decodeBEncode :: ByteString -> BEncode
decodeBEncode bs = case decodeBEncode' bs of
  Just (decodedData, _) -> decodedData
  Nothing -> error $ "Error: Decoding bytestring: " ++ B.unpack bs

encodeBEncode :: BEncode -> ByteString
encodeBEncode (BString bs) = B.concat [B.pack (show (B.length bs)), ":", bs]
encodeBEncode (BInt i) = B.concat ["i", B.pack (show i), "e"]
encodeBEncode (BList xs) = B.concat ["l", B.concat (map encodeBEncode xs), "e"]
encodeBEncode (BDict dict) =
  let sortedDict = sortBy (\(k1, _) (k2, _) -> compare k1 k2) dict
      encodePair (k, v) = B.concat [encodeBEncode (BString k), encodeBEncode v]
   in B.concat ["d", B.concat (map encodePair sortedDict), "e"]
