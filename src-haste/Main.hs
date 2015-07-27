{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Haste.Concurrent
import Haste.Prim
import Haste.WebSockets

main :: IO ()
main = concurrent $ withWebSocket "ws://10.0.0.2:30001/ws" onData onError run
  where onData _ str = liftIO $ putStrLn $ fromJSStr str
        onError = error "Crap"
        run _ = liftIO $ putStrLn "Run"
