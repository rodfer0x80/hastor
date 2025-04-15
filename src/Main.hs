{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import Debug.Trace (trace)
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)


import BEncode (decodeBEncode, BEncode) 


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
            let encodedValue = args !! 1
            case decodeBEncode (B.pack encodedValue) of  
                (decodedValue, _) -> do 
                    let jsonValue = encode decodedValue 
                    LB.putStr jsonValue 
                    putStr "\n"
        _ -> putStrLn $ "Unknown command: " ++ command
