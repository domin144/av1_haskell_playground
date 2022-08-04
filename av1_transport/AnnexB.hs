module AnnexB (decodeBitstream) where

import Common (ObuBytes, decodeLeb128, maybeSplit)
import Data.Bits (Bits (clearBit, shift, testBit))
import Data.Word (Word8)

readObu :: Integer -> [Word8] -> Maybe (ObuBytes, [Word8])
readObu = Common.maybeSplit

decodeFrameUnit :: Integer -> [Word8] -> Maybe ([ObuBytes], [Word8])
decodeFrameUnit 0 xs = Just ([], xs)
decodeFrameUnit size xs = do
  (obuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (obu, xs) <- readObu obuSize xs
  (moreObus, xs) <-
    if leb128Bytes + obuSize <= size
      then decodeFrameUnit (size - leb128Bytes - obuSize) xs
      else Nothing
  return (obu : moreObus, xs)

decodeTemporalUnit :: Integer -> [Word8] -> Maybe ([ObuBytes], [Word8])
decodeTemporalUnit 0 xs = Just ([], xs)
decodeTemporalUnit size xs = do
  (fuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (obus, xs) <- decodeFrameUnit fuSize xs
  (moreObus, xs) <-
    if leb128Bytes + fuSize <= size
      then decodeTemporalUnit (size - leb128Bytes - fuSize) xs
      else Nothing
  return (obus ++ moreObus, xs)

decodeBitstream :: [Word8] -> Maybe [ObuBytes]
decodeBitstream [] = Just []
decodeBitstream xs = do
  (tuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (obus, xs) <- decodeTemporalUnit tuSize xs
  moreObus <- decodeBitstream xs
  return (obus ++ moreObus)
