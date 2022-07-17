module Common
  ( TransportFormat (..),
    Parameters (..),
    ObuBytes,
    ObuType,
    decodeLeb128,
    maybeSplit,
    at
  )
where

import Data.Bits (Bits (clearBit, shift, testBit))
import Data.Word (Word8)

data TransportFormat = LowOverhead | AnnexB | Json
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Parameters = Parameters
  { inputFormat :: TransportFormat,
    inputFileName :: String,
    outputFormat :: TransportFormat,
    outputFileName :: String
  }
  deriving (Eq, Show, Read)

type ObuBytes = [Word8]

data ObuType
  = Reserved0
  | ObuSequenceHeader
  | ObuTemporalDelimiter
  | ObuFrameHeader
  | ObuTileGroup
  | ObuMetadata
  | ObuFrame
  | ObuRedundantFrameHeader
  | ObuTileList
  | Reserved9
  | Reserved10
  | Reserved11
  | Reserved12
  | Reserved13
  | Reserved14
  | ObuPadding
  deriving (Eq, Show, Read, Enum, Bounded)

decodeLeb128 :: [Word8] -> Maybe (Integer, Integer, [Word8])
decodeLeb128 xs = decode xs 0 0
  where
    decode [] _ _ = Nothing
    decode (y : ys) i acc
      | i + 1 < 8 && moreDataFlag = decode ys (i + 1) newAcc
      | not moreDataFlag = Just (newAcc, toInteger (i + 1), ys)
      | otherwise = Nothing
      where
        currentData = toInteger (clearBit y 7)
        newAcc = shift currentData (7 * i) + acc
        moreDataFlag = testBit y 7

maybeSplit :: Integral i => i -> [a] -> Maybe ([a], [a])
maybeSplit n xs
  | n < 0 = Nothing
  | n == 0 = Just ([], xs)
  | otherwise = case xs of
    [] -> Nothing
    y : ys -> case maybeSplit (n - 1) ys of
      Nothing -> Nothing
      Just (ys0, ys1) -> Just (y : ys0, ys1)

at :: Integral a => [b] -> a -> Maybe b
at [] _ = Nothing
at (x : _) 0 = Just x
at (_ : xs) i = at xs (i - 1)