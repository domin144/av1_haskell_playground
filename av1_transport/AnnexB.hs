module AnnexB (decodeBitstream, flattenTheBitstream) where

import Common (ObuBytes, ObuType (..), decodeLeb128, maybeSplit)
import Data.Bits (Bits (clearBit, shift, testBit))
import Data.Word (Word8)

type FrameUnit = [ObuBytes]

type TemporalUnit = [FrameUnit]

type TypedObu = (ObuType, ObuBytes)

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

getObuType :: ObuBytes -> Maybe ObuType
getObuType = error "TODO"

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
