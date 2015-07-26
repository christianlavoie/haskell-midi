module Main (main) where

import Text.Printf

import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)
import System.Environment (getArgs)

import qualified Control.Concurrent.Chan as Chan
import qualified Data.List.Split as Split
import qualified System.IO as IO

import Parser

main :: IO ()
main = do
    args <- getArgs
    chan <- Chan.newChan

    forM_ args $ \arg -> do
        case Split.splitOn ":" arg of
            ["source", name, path] -> do
                putStrLn $ printf "Connecting source %s to %s" name path
                forkIO $ IO.withFile path IO.ReadMode $ Parser.process chan name

            other -> do 
                error $ printf "Unrecognized %s" (show other)

    forever $ do
        msg <- Chan.readChan chan
        putStrLn $ msg
