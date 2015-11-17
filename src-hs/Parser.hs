{-# LANGUAGE BangPatterns #-}

module Parser (message, process) where

import Data.Bits
import Text.Printf

import Control.Monad (liftM, liftM2)

import qualified Control.Concurrent.Chan as Chan
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as ByteString
import qualified System.IO as IO

import Midi

message :: AP.Parser MidiMessage
message = AP.choice parsers
  where parsers = [ p_note_off, p_note_on, p_polyphonic_key_pressure,
                    p_control_change, p_program_change, p_channel_pressure,
                    p_pitch_bend_change ]

        satisfy_four_bits m = do
            b1 <- AP.satisfy (\b -> b .&. 0xf0 == m)
            return $ b1 .&. 0x0f

        simple_1_byte first_four constructor = do
            b1 <- satisfy_four_bits first_four
            liftM (constructor b1) AP.anyWord8

        simple_2_bytes first_four constructor = do
            b1 <- satisfy_four_bits first_four
            liftM2 (constructor b1) AP.anyWord8 AP.anyWord8

        p_note_off                = simple_2_bytes 0x80 NoteOff
        p_note_on                 = simple_2_bytes 0x90 NoteOn
        p_polyphonic_key_pressure = simple_2_bytes 0xa0 PolyphonicKeyPressure
        p_control_change          = simple_2_bytes 0xb0 ControlChange
        p_program_change          = simple_1_byte  0xc0 ProgramChange
        p_channel_pressure        = simple_1_byte  0xd0 ChannelPressure
        p_pitch_bend_change       = simple_2_bytes 0xe0 PitchBendChange

process :: Chan.Chan String -> String -> IO.Handle -> IO ()
process chan name handle = do
    bytes <- refill handle
    go (AP.parse message bytes)

  where refill h = ByteString.hGet h 1

        go (AP.Fail _ _ msg) = error $ "AP.Fail: " ++ msg

        go cont@(AP.Partial _) = do
             bytes <- refill handle
             go $ AP.feed cont bytes

        go (AP.Done bytes (NoteOn w1 w2 w3)) = do
             Chan.writeChan chan $ printf "%s:%d:%d:%d" name w1 w2 w3
             go $ AP.parse message bytes

        go (AP.Done bytes msg) = do
             putStrLn $ printf "UI doesn't understand message %s yet" (show msg)
             go $ AP.parse message bytes
