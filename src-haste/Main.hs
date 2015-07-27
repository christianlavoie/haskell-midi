{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Haste.Concurrent
import Haste.DOM
import Haste.Foreign
import Haste.Graphics.Canvas
import Haste.Prim
import Haste.WebSockets

import qualified Data.List as DL
import qualified Data.Map as DM

main :: IO ()
main = concurrent $ withWebSocket "ws://192.168.60.131:30001/ws" onData onError run
  where onError = error "Crap"
        run _ = liftIO $ putStrLn "Run"

channelMap :: DM.Map Int (Double, Double)
channelMap = DM.fromList [
    -- First row
    (49, (0, 0)),
    (57, (1, 0)),
    (51, (2, 0)),
    (53, (3, 0)),

    -- Second row
    (56, (0, 1)),
    (39, (1, 1)),
    (42, (2, 1)),
    (46, (3, 1)),

    -- Third row
    (50, (0, 2)),
    (48, (1, 2)),
    (45, (2, 2)),
    (41, (3, 2)),

    -- Fourth row
    (36, (0, 3)),
    (38, (1, 3)),
    (40, (2, 3)),
    (37, (3, 3)) ]

onData :: WebSocket -> JSString -> CIO ()
onData _ str = liftIO $ do
    consoleLog str
    Just canvasElem <- elemById "drawArea"
    width <- getProp canvasElem "width"
    height <- getProp canvasElem "height"
    Just canvas <- getCanvas canvasElem
    let [_, _, channel, velocity] = removeSemis (fromJSStr str)

    let (wIdx, hIdx) = channelMap DM.! read channel
    renderSquare canvas (read width) (read height) wIdx hIdx (read velocity)

  where consoleLog :: JSString -> IO ()
        consoleLog = ffi "(function (x) { console.log('received', x); })"

        renderSquare :: Canvas -> Double -> Double -> Double -> Double -> Int -> IO ()
        renderSquare canvas width height wIdx hIdx velocity = do
            let topLeft = (wIdx * width / 4, hIdx * height / 4)
            let bottomRight = ((1 + wIdx) * width / 4, (1 + hIdx) * height / 4)

            let colour = maximum [255 - 2 * velocity, 0]
            renderOnTop canvas $ color (RGB 255 colour colour) $ fill $ rect topLeft bottomRight

        removeSemis :: String -> [String]
        removeSemis str = map (filter (/= ':')) $ DL.groupBy (\x y -> y /= ':') str
