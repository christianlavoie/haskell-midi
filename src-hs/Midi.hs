module Midi (MidiMessage(..)) where

import Control.Monad (liftM2, liftM3)

import Data.Bits
import Data.Word
import Data.Serialize
import Text.Printf

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
        case w1 .&. 0xf0 of
            0x80 -> getThreeBytes NoteOff w1
            0x90 -> getThreeBytes NoteOn w1
            0xa0 -> getThreeBytes PolyphonicKeyPressure w1
            0xb0 -> getThreeBytes ControlChange w1
            0xc0 -> getTwoBytes ProgramChange w1
            0xd0 -> getTwoBytes ChannelPressure w1
            0xe0 -> getThreeBytes PitchBendChange w1
            other -> error $ printf "Midi message cannot start with 0x%02x" other

    put (NoteOff w1 w2 w3) = putThreeBytes 0x80 w1 w2 w3
    put (NoteOn w1 w2 w3) = putThreeBytes 0x90 w1 w2 w3
    put (PolyphonicKeyPressure w1 w2 w3) = putThreeBytes 0xa0 w1 w2 w3
    put (ControlChange w1 w2 w3) = putThreeBytes 0xb0 w1 w2 w3
    put (ProgramChange w1 w2) = putTwoBytes 0xc0 w1 w2
    put (ChannelPressure w1 w2) = putTwoBytes 0xd0 w1 w2
    put (PitchBendChange w1 w2 w3) = putThreeBytes 0xe0 w1 w2 w3

getTwoBytes :: (Word8 -> Word8 -> MidiMessage) -> Word8 -> Get MidiMessage
getTwoBytes constructor w1 = do
    w2 <- get
    return $ constructor (w1 .&. 0x0f) w2

getThreeBytes :: (Word8 -> Word8 -> Word8 -> MidiMessage) -> Word8 -> Get MidiMessage
getThreeBytes constructor w1 = do
    w2 <- get
    w3 <- get
    return $ constructor (w1 .&. 0x0f) w2 w3

putTwoBytes :: Word8 -> Word8 -> Word8 -> PutM ()
putTwoBytes mask w1 w2 = do
    put $ w1 .|. mask
    put w2

putThreeBytes :: Word8 -> Word8 -> Word8 -> Word8 -> PutM ()
putThreeBytes mask w1 w2 w3 = do
    put $ w1 .|. mask
    put w2
    put w3

arbitraryRange :: [Word8] -> QC.Gen Word8
arbitraryRange = QC.oneof . map return

arbitraryChannel :: QC.Gen Word8
arbitraryChannel = arbitraryRange [0 .. 15]

arbitraryProgram :: QC.Gen Word8
arbitraryProgram = arbitraryRange [0 .. 127]

arbitraryNote :: QC.Gen Word8
arbitraryNote = arbitraryRange [0 .. 127]

arbitraryVelocity :: QC.Gen Word8
arbitraryVelocity = arbitraryRange [0 .. 127]

instance QC.Arbitrary MidiMessage where
    arbitrary = QC.oneof [
        liftM3 NoteOff arbitraryChannel arbitraryNote arbitraryVelocity,
        liftM3 NoteOn arbitraryChannel arbitraryNote arbitraryVelocity,
        liftM3 PolyphonicKeyPressure arbitraryChannel arbitraryNote arbitraryVelocity,
        liftM3 ControlChange arbitraryChannel arbitraryNote arbitraryVelocity,
        liftM2 ProgramChange arbitraryChannel arbitraryProgram,
        liftM2 ChannelPressure arbitraryChannel arbitraryVelocity,
        liftM3 PitchBendChange arbitraryChannel arbitraryNote arbitraryVelocity ]
