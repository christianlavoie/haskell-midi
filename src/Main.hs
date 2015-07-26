{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Bits
import Data.Word

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as ByteString
import qualified System.IO as IO

data MidiMessage = NoteOff Word8 Word8 Word8
                 | NoteOn Word8 Word8 Word8
                 | PolyphonicKeyPressure Word8 Word8 Word8
                 | ControlChange Word8 Word8 Word8
                 | ProgramChange Word8 Word8
                 | ChannelPressure Word8 Word8
                 | PitchBendChange Word8 Word8 Word8
  deriving (Eq, Ord, Show)

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

main :: IO ()
main = do
    bytes <- refill IO.stdin
    process (AP.parse message bytes)

  where refill h = ByteString.hGet h 1

        process (AP.Fail _ _ msg) = error $ "AP.Fail: " ++ msg

        process cont@(AP.Partial _) = do
                     bytes <- refill IO.stdin
                     process $ AP.feed cont bytes

        process (AP.Done bytes msg) = do
                     putStrLn $ "Done: " ++ show msg
                     process $ AP.parse message bytes
