module AnnexB
  ( decodeBitstream,
    encodeBitstream,
    decodeStructuredBitstream,
    encodeStructuredBitstream,
    groupTheBistream,
    flattenTheBitstream,
  )
where

import BytestreamLayer
  ( FrameUnit,
    TemporalUnit,
    TypedObu,
  )
import Common
  ( ObuBytes,
    ObuType (..),
    decodeLeb128,
    encodeLeb128,
    maybeSplit,
    rewindToRight,
  )
import Data.List (concat)
import Data.Word (Word8)
import ObuHeader (ObuHeader (..), bytesToBits, decodeObuHeader)

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

decodeStructuredBitstream :: [Word8] -> Maybe [TemporalUnit]
decodeStructuredBitstream [] = Just []
decodeStructuredBitstream xs = do
  (tuSize, leb128Bytes, xs) <- decodeLeb128 xs
  (temporalUnit, xs) <- decodeTemporalUnit tuSize xs
  moreTus <- decodeStructuredBitstream xs
  return (temporalUnit : moreTus)

flattenTheBitstream :: [TemporalUnit] -> [ObuBytes]
flattenTheBitstream = concat . concat

decodeBitstream :: [Word8] -> Maybe [ObuBytes]
decodeBitstream xs = do
  structuredBitstream <- decodeStructuredBitstream xs
  return $ flattenTheBitstream structuredBitstream

getObuType :: ObuBytes -> Maybe ObuType
getObuType obuBytes = do
  (header, _) <- case decodeObuHeader $ bytesToBits obuBytes of
    Right x -> Just x
    Left _ -> Nothing
  return (obuType header)

addTypeToObus :: [ObuBytes] -> Maybe [TypedObu]
addTypeToObus = mapM obuToTypedObu
  where
    obuToTypedObu x = do
      obuType <- getObuType x
      return (obuType, x)

splitWithHead :: (a -> Bool) -> [a] -> Maybe [[a]]
splitWithHead _ [] = Just []
splitWithHead isHead (x : xs) =
  if isHead x
    then readMore [x] xs
    else Nothing
  where
    readMore acc [] = Just [reverse acc]
    readMore acc (x : xs) =
      if isHead x
        then do
          let group = reverse acc
          moreGroups <- readMore [x] xs
          return (group : moreGroups)
        else readMore (x : acc) xs

isTemporalDelimiter :: TypedObu -> Bool
isTemporalDelimiter (obuType, _) = obuType == ObuTemporalDelimiter

isFrameStart :: TypedObu -> Bool
isFrameStart (obuType, _) = obuType == ObuFrameHeader || obuType == ObuFrame

isTileData :: TypedObu -> Bool
isTileData (obuType, _) = obuType == ObuTileGroup

splitToTus :: [TypedObu] -> Maybe [[TypedObu]]
splitToTus = splitWithHead isTemporalDelimiter

splitToFus :: [TypedObu] -> Maybe [[TypedObu]]
splitToFus obus =
  step [] [] obus
  where
    -- step tus middle obus
    -- Variable middle is the part, which is not yet decided, which FU to go.
    -- Variable middle can never contain a frame header.
    step :: [[TypedObu]] -> [TypedObu] -> [TypedObu] -> Maybe [[TypedObu]]
    step [] [] [] = Just []
    step [] middle [] = Nothing
    -- Variable middle may be empty here.
    step (tu : tus) middle [] =
      Just $ reverse $ map reverse ((middle ++ tu) : tus)
    step tus middle (obu : obus)
      | isFrameStart obu =
        step ((obu : middle) : tus) [] obus
      | isTileData obu =
        case tus of
          [] -> Nothing
          tu : tus -> step ((obu : middle ++ tu) : tus) [] obus
      | otherwise =
        step tus (obu : middle) obus

groupTheBistream :: [ObuBytes] -> Maybe [TemporalUnit]
groupTheBistream obus = do
  typedObus <- addTypeToObus obus
  tus <- splitToTus typedObus
  typedBitstream <- mapM splitToFus tus
  return $ map (map (map snd)) typedBitstream

encodeFrameUnit :: FrameUnit -> Maybe ([Word8], Integer)
encodeFrameUnit obus =
  --- step acc sz obus, where
  ---   acc is reversed output
  ---   sz is size of output
  ---   obus are OBU's which are left to be processed
  step [] 0 obus
  where
    step :: [Word8] -> Integer -> [ObuBytes] -> Maybe ([Word8], Integer)
    step bytes sz [] = Just (reverse bytes, sz)
    step bytes sz (obu : obus) = do
      let obuLength = toInteger $ length obu
      (codedObuLength, codedObuLengthLength) <- encodeLeb128 obuLength
      step
        (rewindToRight obu (rewindToRight codedObuLength bytes))
        (sz + codedObuLengthLength + obuLength)
        obus

encodeTemporalUnit :: TemporalUnit -> Maybe ([Word8], Integer)
encodeTemporalUnit fus =
  step [] 0 fus
  where
    step :: [Word8] -> Integer -> [FrameUnit] -> Maybe ([Word8], Integer)
    step bytes sz [] = Just (reverse bytes, sz)
    step bytes sz (fu : fus) = do
      (codedFu, lengthOfCodedFu) <- encodeFrameUnit fu
      (codedLengthOfCodedFu, lengthOfCodedLengthOfCodedFu) <-
        encodeLeb128 lengthOfCodedFu
      step
        (rewindToRight codedFu (rewindToRight codedLengthOfCodedFu bytes))
        (sz + lengthOfCodedFu + lengthOfCodedLengthOfCodedFu)
        fus

encodeStructuredBitstream :: [TemporalUnit] -> Maybe [Word8]
encodeStructuredBitstream tus =
  step [] 0 tus
  where
    step :: [Word8] -> Integer -> [TemporalUnit] -> Maybe [Word8]
    step bytes sz [] = Just (reverse bytes)
    step bytes sz (tu : tus) = do
      (codedTu, lengthOfCodedTu) <- encodeTemporalUnit tu
      (codedLengthOfCodedTu, lengthOfCodedLengthOfCodedTu) <-
        encodeLeb128 lengthOfCodedTu
      step
        (rewindToRight codedTu (rewindToRight codedLengthOfCodedTu bytes))
        (sz + lengthOfCodedTu + lengthOfCodedLengthOfCodedTu)
        tus

encodeBitstream :: [ObuBytes] -> Maybe [Word8]
encodeBitstream xs = do
  structuredBitstream <- groupTheBistream xs
  encodeStructuredBitstream structuredBitstream