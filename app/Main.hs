module Main where

import           System.Environment                 ( getArgs )
import qualified Data.ByteString.Base64     as B64  ( encode )
import           Text.Printf ( printf )
import           Data.ByteString            as BS   ( ByteString
                                                    , readFile )
import           SDLTricksCube.Launch ( launch, launch_ )

type Log = String -> IO ()

main :: IO ()
main = do
    args <- getArgs
    launch_ (info, warn, err) args where
    info = log' "INFO"
    warn = log' "WARN"
    err = log' "ERROR"
    log' x = putStrLn . printf "__android_log_write: %s: %s" x

-- imageBase64 = BS.readFile "assets/nefeli.png.base64"
