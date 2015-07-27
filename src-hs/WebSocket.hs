{-# LANGUAGE OverloadedStrings #-}

module WebSocket (process) where

import Text.Printf

import Control.Monad (forever)

import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Network.WebSockets as WS

process :: Chan.Chan String -> String -> String -> Int -> IO ()
process chan name addr port = WS.runServer addr port (app chan name)

app :: Chan.Chan String -> String -> WS.ServerApp
app chan name pendingConn = forever $ do
    conn <- WS.acceptRequest pendingConn
    putStrLn "Received connection"
    forever $ do
        msg <- Chan.readChan chan
        putStrLn $ printf "WebSocket (%s) sending %s to browser" name msg
        WS.sendDataMessage conn (WS.Text $ BS.pack msg)
