module AnnexB (decodeBitstream) where

import Common (ObuBytes, decodeLeb128, maybeSplit)
import Data.Bits (Bits (clearBit, shift, testBit))
import Data.Word (Word8)

type FrameUnit = [ObuBytes]
type TemporalUnit = [FrameUnit]

readObu :: Integer -> [Word8] -> Maybe (ObuBytes, [Word8])
readObu = Common.maybeSplit

decodeFrameUnit :: Integer -> [Word8] -> Maybe (FrameUnit, [Word8])
decodeFrameUnit 0 xs = Just ([], xs)
decodeFrameUnit size xs = do
  (obuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (obu, xs) <- readObu obuSize xs
  (moreObus, xs) <-
    if leb128Bytes + obuSize <= size
      then decodeFrameUnit (size - leb128Bytes - obuSize) xs
      else Nothing
  return (obu : moreObus, xs)

decodeTemporalUnit :: Integer -> [Word8] -> Maybe (TemporalUnit, [Word8])
decodeTemporalUnit 0 xs = Just ([], xs)
decodeTemporalUnit size xs = do
  (fuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (frameUnit, xs) <- decodeFrameUnit fuSize xs
  (moreFus, xs) <-
    if leb128Bytes + fuSize <= size
      then decodeTemporalUnit (size - leb128Bytes - fuSize) xs
      else Nothing
  return (frameUnit : moreFus, xs)

decodeBitstream :: [Word8] -> Maybe [TemporalUnit]
decodeBitstream [] = Just []
decodeBitstream xs = do
  (tuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (temporalUnit, xs) <- decodeTemporalUnit tuSize xs
  moreTus <- decodeBitstream xs
  return (temporalUnit : moreTus)

flattenTheBitstream :: [TemporalUnit] -> [ObuBytes]
flattenTheBitstream = concat . concat