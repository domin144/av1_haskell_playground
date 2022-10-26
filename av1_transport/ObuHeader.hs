module ObuHeader (ObuHeader (..), bytesToBits, decodeFixed, decodeObuHeader) where

import Common (ObuBytes, ObuType, at)
import Data.Bits (setBit, testBit)
import Data.Word (Word8)

data ObuHeader = ObuHeader
  { obuType :: ObuType,
    obuExtensionFlag :: Bool,
    obuHasSizeField :: Bool,
    temporalId :: Integer,
    spatialId :: Integer
  }

-- AV1 uses the awful scheme of reading bytes from the MSB side
bytesToBits :: [Word8] -> [Bool]
bytesToBits [] = []
bytesToBits (x : xs) = byteToBits x ++ bytesToBits xs
  where
    byteToBits x = [testBit x n | n <- [7, 6 .. 0]]

decodeFixed :: Integer -> [Bool] -> Maybe (Integer, [Bool])
decodeFixed 0 xs = Just (0, xs)
decodeFixed _ [] = Nothing
decodeFixed n (x : xs) = case decodeFixed (n - 1) xs of
  Nothing -> Nothing
  Just (y, xsLeft) ->
    Just (if x then setBit y (fromInteger n - 1) else y, xsLeft)

integerToObuType :: Integer -> Maybe ObuType
integerToObuType i = [(minBound :: ObuType) ..] `at` i

decodeObuHeader :: [Bool] -> Maybe (ObuHeader, [Bool])
decodeObuHeader xs = do
  (obuForbiddenBit, xs) <- decodeFixed 1 xs
  if obuForbiddenBit == 0
    then Just ()
    else Nothing
  (obuType, xs) <- decodeFixed 4 xs
  obuType <- integerToObuType obuType
  (obuExtensionFlag, xs) <- decodeFixed 1 xs
  (obuHasSizeField, xs) <- decodeFixed 1 xs
  (obuReserved1Bit, xs) <- decodeFixed 1 xs
  if obuReserved1Bit == 1
    then Just ()
    else Nothing
  if obuExtensionFlag == 1
    then do
      (temporalId, xs) <- decodeFixed 3 xs
      (spatialId, xs) <- decodeFixed 2 xs
      (extensionHeaderReserved3Bits, xs) <- decodeFixed 3 xs
      if extensionHeaderReserved3Bits == 0
        then Just ()
        else Nothing
      return
        ( ObuHeader
            { obuType = obuType,
              obuExtensionFlag = obuExtensionFlag == 1,
              obuHasSizeField = obuHasSizeField == 1,
              temporalId = temporalId,
              spatialId = spatialId
            },
          xs
        )
    else
      return
        ( ObuHeader
            { obuType = obuType,
              obuExtensionFlag = obuExtensionFlag == 1,
              obuHasSizeField = obuHasSizeField == 1,
              temporalId = 0,
              spatialId = 0
            },
          xs
        )