module AnnexB (decodeBitstream) where

import Common (ObuBytes, decodeLeb128, maybeSplit)
import Data.Bits (Bits (clearBit, shift, testBit))
import Data.Word (Word8)

readObu :: Integer -> [Word8] -> Maybe (ObuBytes, [Word8])
readObu = Common.maybeSplit

decodeFrameUnit :: Integer -> [Word8] -> Maybe ([ObuBytes], [Word8])
decodeFrameUnit 0 xs = Just ([], xs)
decodeFrameUnit size xs = do
  (obuSize, leb128Bytes, xs2) <- decodeLeb128 xs
  (obu, xs3) <- readObu obuSize xs2
  (moreObus, xs4) <-
    if leb128Bytes + obuSize <= size
      then decodeFrameUnit (size - leb128Bytes - obuSize) xs3
      else Nothing
  return (obu : moreObus, xs4)

decodeTemporalUnit :: Integer -> [Word8] -> Maybe ([ObuBytes], [Word8])
decodeTemporalUnit 0 xs = Just ([], xs)
decodeTemporalUnit size xs = do
  (fuSize, leb128Bytes, xs2) <- decodeLeb128 xs
  (obus, xs3) <- decodeFrameUnit fuSize xs2
  (moreObus, xs4) <-
    if leb128Bytes + fuSize <= size
      then decodeTemporalUnit (size - leb128Bytes - fuSize) xs3
      else Nothing
  return (obus ++ moreObus, xs4)

decodeBitstream :: [Word8] -> Maybe [ObuBytes]
decodeBitstream [] = Just []
decodeBitstream xs = do
  (tuSize, leb128Bytes, xs2) <- decodeLeb128 xs
  (obus, xs3) <- decodeTemporalUnit tuSize xs2
  moreObus <- decodeBitstream xs3
  return (obus ++ moreObus)
