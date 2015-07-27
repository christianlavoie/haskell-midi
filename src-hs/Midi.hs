module Midi where

import Data.Bits
import Data.Word
import Data.Serialize

import qualified Test.QuickCheck as QC

data MidiMessage = NoteOff Word8 Word8 Word8
                 | NoteOn Word8 Word8 Word8
                 | PolyphonicKeyPressure Word8 Word8 Word8
                 | ControlChange Word8 Word8 Word8
                 | ProgramChange Word8 Word8
                 | ChannelPressure Word8 Word8
                 | PitchBendChange Word8 Word8 Word8
  deriving (Eq, Ord, Show)

instance Serialize MidiMessage where
    get = do
        w1 <- get
        w2 <- get
        w3 <- get
        return $ case w1 .&. 0xf0 of
            0x80 -> NoteOff (w1 .&. 0x0f) w2 w3
            _ -> error "Unimplemented"

    put (NoteOff w1 w2 w3) = do
        put (w1 .|. 0x80)
        put w2
        put w3

    put _ = error "Unimplemented"

instance QC.Arbitrary MidiMessage where
    arbitrary = do
        w1 <- QC.oneof $ map return [0 .. 15]
        w2 <- QC.oneof $ map return [0 .. 127]
        w3 <- QC.oneof $ map return [0 .. 127]
        return $ NoteOff w1 w2 w3
