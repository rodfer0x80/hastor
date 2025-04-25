{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import BEncode (BEncode (BDict), decodeBEncode)
import Meta (parseMeta, getPeers, requestPeers)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

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
        decodedData -> do
          LB.putStr $ encode decodedData
          putStrLn ""
    "parse" -> do
      let metaFile = args !! 1
      metaFileContent <- BS.readFile metaFile
      case decodeBEncode metaFileContent of
        decodedData -> putStr $ parseMeta decodedData
    "requestPeers" -> do
      let metaFile = args !! 1
      metaFileContent <- BS.readFile metaFile
      case decodeBEncode metaFileContent of
        decodedData -> do 
          result <- requestPeers decodedData
          putStr result
    "getPeers" -> do
      let metaFile = args !! 1
      metaFileContent <- BS.readFile metaFile
      case decodeBEncode metaFileContent of
        decodedData -> putStr $ getPeers decodedData
    _ -> putStrLn $ "Unknown command: " ++ command
