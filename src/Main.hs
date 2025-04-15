{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS 
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

import Debug.Trace (trace)
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

import BEncode (decodeBEncode, BEncode (..))


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


main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    args <- getArgs
    when (length args < 2) do
        putStrLn "Usage: your_bittorrent.sh <command> <args>"
        exitWith (ExitFailure 1)

    let command = args !! 0
    case command of
        "decode" -> do
            let encodedData = args !! 1
            case decodeBEncode (B.pack encodedData) of  
                (decodedData, _) -> do 
                    let jsonData = encode decodedData 
                    LB.putStr jsonData 
                    putStr "\n"
        "parse" -> do 
            let metainfoFile = args !! 1
            metainfoFileContent <- BS.readFile metainfoFile 
            case decodeBEncode metainfoFileContent  of
                (BDict topLevel, _) -> do
                    let trackerUrlMaybe :: Maybe BEncode
                        trackerUrlMaybe = lookupBEncode "announce" topLevel
                    let trackerUrl :: Maybe ByteString
                        trackerUrl = trackerUrlMaybe >>= extractString
                    putStrLn $ "Tracker URL: " ++ fromMaybe "N/A" (B.unpack <$> trackerUrl)
                    let infoDictMaybeBEncode :: Maybe BEncode
                        infoDictMaybeBEncode = lookupBEncode "info" topLevel
                    let infoDictMaybe :: Maybe [(ByteString, BEncode)]
                        infoDictMaybe = infoDictMaybeBEncode >>= extractDict
                    case infoDictMaybe of
                        Just infoDict -> do
                            case lookupBEncode "length" infoDict >>= extractString of
                                Just lengthStr -> putStrLn $ "Length: " ++ B.unpack lengthStr
                                Nothing -> case lookupBEncode "length" infoDict >>= extractInteger of
                                    Just lengthInt -> putStrLn $ "Length: " ++ show lengthInt
                                    Nothing -> putStrLn "Length: N/A"
                        Nothing -> putStrLn "Info dictionary not found"
                _ -> putStrLn "Error decoding top-level dictionary"
        _ -> putStrLn $ "Unknown command: " ++ command
