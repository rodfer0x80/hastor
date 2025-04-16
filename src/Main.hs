{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS

import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

import BEncode (decodeBEncode, BEncode (BDict))
import Metainfo (parseMetainfo)


main :: IO ()
main = do
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
                decodedData  -> do
                    LB.putStr $ encode decodedData 
                    putStrLn ""
        "parse" -> do
            let metainfoFile = args !! 1
            metainfoFileContent <- BS.readFile metainfoFile
            case decodeBEncode metainfoFileContent of
                decodedData -> putStr $ parseMetainfo decodedData
        _ -> putStrLn $ "Unknown command: " ++ command
