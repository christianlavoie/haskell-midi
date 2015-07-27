module Main (main) where

import Text.Printf

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_)
import System.Environment (getArgs)

import qualified Control.Concurrent.Chan as Chan
import qualified Data.List.Split as Split
import qualified System.IO as IO

import qualified Parser
import qualified WebSocket

main :: IO ()
main = do
    args <- getArgs
    chan <- Chan.newChan

    forM_ args $ \arg -> do
        case Split.splitOn ":" arg of
            ["source", name, path] -> do
                putStrLn $ printf "Connecting source %s to %s" name path
                forkIO $ IO.withFile path IO.ReadMode $ Parser.process chan name

            ["websocket", name, host, port_] -> do
                let port = read port_
                putStrLn $ printf "Firing up websocket listener on %s:%d" host port
                forkIO $ WebSocket.process chan name host port

            other -> do 
                error $ printf "Unrecognized %s" (show other)

    forever $ threadDelay 500000
