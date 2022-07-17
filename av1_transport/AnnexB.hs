module AnnexB (decodeBitstream) where

import Common (ObuBytes, decodeLeb128)
import Data.Bits (Bits (clearBit, shift, testBit))
import Data.Word (Word8)

readObu :: Integer -> [Word8] -> Maybe (ObuBytes, [Word8])
readObu = maybeSplit

decodeFrameUnit :: Integer -> [Word8] -> Maybe ([ObuBytes], [Word8])
decodeFrameUnit 0 [] = Just ([], [])
decodeFrameUnit size xs =
  case decodeLeb128 xs of
    Nothing -> Nothing
    Just (obuSize, leb128Bytes, xs2) ->
      case readObu obuSize xs2 of
        Nothing -> Nothing
        Just (obu, xs3) ->
          if leb128Bytes + obuSize <= size
            then case decodeFrameUnit (size - leb128Bytes - obuSize) xs3 of
              Nothing -> Nothing
              Just (moreObus, xs4) -> Just (obu : moreObus, xs4)
            else Nothing

decodeTemporalUnit :: Integer -> [Word8] -> Maybe ([ObuBytes], [Word8])
decodeTemporalUnit 0 [] = Just ([], [])
decodeTemporalUnit size xs =
  case decodeLeb128 xs of
    Nothing -> Nothing
    Just (fuSize, leb128Bytes, xs2) ->
      case decodeFrameUnit fuSize xs2 of
        Nothing -> Nothing
        Just (obus, xs3) ->
          if leb128Bytes + fuSize <= size
            then case decodeTemporalUnit (size - leb128Bytes - fuSize) xs3 of
              Nothing -> Nothing
              Just (moreObus, xs4) -> Just (obus ++ moreObus, xs4)
            else Nothing

decodeBitstream :: [Word8] -> Maybe [ObuBytes]
decodeBitstream [] = Just []
decodeBitstream xs =
  case decodeLeb128 xs of
    Nothing -> Nothing
    Just (tuSize, leb128Bytes, xs2) ->
      case decodeTemporalUnit tuSize xs2 of
        Nothing -> Nothing
        Just (obus, xs3) ->
          case decodeBitstream xs3 of
            Nothing -> Nothing
            Just moreObus -> Just (obus ++ moreObus)
