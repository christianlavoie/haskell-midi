module Midi where

import Data.Word

data MidiMessage = NoteOff Word8 Word8 Word8
                 | NoteOn Word8 Word8 Word8
                 | PolyphonicKeyPressure Word8 Word8 Word8
                 | ControlChange Word8 Word8 Word8
                 | ProgramChange Word8 Word8
                 | ChannelPressure Word8 Word8
                 | PitchBendChange Word8 Word8 Word8
  deriving (Eq, Ord, Show)
