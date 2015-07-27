{-# LANGUAGE BangPatterns #-}

module Parser (process) where

import Data.Bits
import Text.Printf

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

        p_note_off = do
            b1 <- satisfy_four_bits 0x80
            b2 <- AP.anyWord8
            b3 <- AP.anyWord8
            return $ NoteOff b1 b2 b3

        p_note_on = do
            b1 <- satisfy_four_bits 0x90
            b2 <- AP.anyWord8
            b3 <- AP.anyWord8
            return $ NoteOn b1 b2 b3

        p_polyphonic_key_pressure = do
            b1 <- satisfy_four_bits 0xa0
            b2 <- AP.anyWord8
            b3 <- AP.anyWord8
            return $ PolyphonicKeyPressure b1 b2 b3

        p_control_change = do
            b1 <- satisfy_four_bits 0xb0
            b2 <- AP.anyWord8
            b3 <- AP.anyWord8
            return $ ControlChange b1 b2 b3

        p_program_change = do
            b1 <- satisfy_four_bits 0xc0
            b2 <- AP.anyWord8
            return $ ProgramChange b1 b2

        p_channel_pressure = do
            b1 <- satisfy_four_bits 0xd0
            b2 <- AP.anyWord8
            return $ ChannelPressure b1 b2

        p_pitch_bend_change = do
            b1 <- satisfy_four_bits 0xe0
            b2 <- AP.anyWord8
            b3 <- AP.anyWord8
            return $ PitchBendChange b1 b2 b3

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
