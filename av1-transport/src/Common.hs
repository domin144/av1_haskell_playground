module Common
  ( TransportFormat (..),
    Parameters (..),
    ObuBytes,
    ObuType (..),
    decodeLeb128,
    encodeLeb128,
    maybeSplit,
    at,
    rewindToLeft,
    rewindToRight,
    Result,
    wrapResult,
    wrapMaybe,
  )
where

import Data.Bits (Bits (bit, clearBit, shift, testBit, (.&.), (.|.)))
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

encodeLeb128 :: Integer -> Maybe ([Word8], Integer)
encodeLeb128 x =
  encode [] x 0
  where
    encode :: [Word8] -> Integer -> Integer -> Maybe ([Word8], Integer)
    encode bytes x i
      | x < bit 7 = Just (reverse (fromInteger x : bytes), i + 1)
      | i + 1 < 8 =
        let currentData = (x .&. (bit 7 - 1)) .|. bit 7
         in encode (fromInteger currentData : bytes) (shift x (-7)) (i + 1)
      | otherwise = Nothing

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

rewindToLeft :: [a] -> [a] -> [a]
rewindToLeft = foldl (flip (:))

rewindToRight :: [a] -> [a] -> [a]
rewindToRight = flip rewindToLeft

type Result = Either String

wrapResult :: String -> Result a -> Result a
wrapResult comment (Left message) = Left (comment ++ ": " ++ message)
wrapResult _ (Right result) = Right result

wrapMaybe :: String -> Maybe a -> Result a
wrapMaybe comment Nothing = Left comment
wrapMaybe _ (Just result) = Right result