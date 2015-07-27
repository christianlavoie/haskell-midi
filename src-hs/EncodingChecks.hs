module Main where

-- Core and third party modules
import System.Environment (withArgs)
import Text.Printf (printf)

-- import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.Serialize as Bin

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions

import Midi
import Parser

-- prop_MidiMessageJSONEncoding :: MidiMessage -> Result
-- prop_MidiMessageJSONEncoding s = Right s ==? (JSON.eitherDecode . JSON.encode) s

prop_MidiMessageBinaryEncoding :: MidiMessage -> Result
prop_MidiMessageBinaryEncoding s = Right s ==? (Bin.decode . Bin.encode) s

prop_MidiMessageParsing :: MidiMessage -> Result
prop_MidiMessageParsing s = s ==? extract ((AP.parse Parser.message . Bin.encode) s)
  where extract (AP.Done str r) | str == BS.empty = r
        extract (AP.Done str r) = error $ printf "Leftover binary string: %s (parsed: %s)" (show str) (show r)
        extract (AP.Partial _)  = error "Got a partial result"
        extract (AP.Fail str ctxts err)  = error $ printf "Got a Fail %s %s %s result" (show str) (show ctxts) err

main :: IO ()
main = do
    withArgs [] $ hspec $ parallel $ do
        -- describe "json encoding, objects" $ do
        --     prop "MidiMessage encoding" prop_MidiMessageJSONEncoding

        describe "cereal encoding, objects" $ do
            prop "MidiMessage encoding" prop_MidiMessageBinaryEncoding

        describe "attoparsec decoding, objects" $ do
            prop "MidiMessage parsing" prop_MidiMessageParsing
