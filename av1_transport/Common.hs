module Common (TransportFormat (..), Parameters (..), decodeLeb128) where

import Data.Word(Word8)
import Data.Bits (Bits(shift, clearBit, testBit))

data TransportFormat = LowOverhead | AnnexB
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Parameters = Parameters
  { inputFormat :: TransportFormat,
    inputFileName :: String,
    outputFormat :: TransportFormat,
    outputFileName :: String
  }
  deriving (Eq, Show, Read)

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