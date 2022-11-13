module ObuHeader
  ( ObuHeader (..),
    bytesToBits,
    bitsToBytes,
    decodeFixed,
    encodeFixed,
    decodeObuHeader,
    encodeObuHeader,
    headerSize,
  )
where

import Common (ObuBytes, ObuType, Result, at, wrapMaybe, wrapResult)
import Data.Bits (clearBit, setBit, testBit)
import Data.Word (Word8)

data ObuHeader = ObuHeader
  { obuType :: ObuType,
    obuExtensionFlag :: Bool,
    obuHasSizeField :: Bool,
    temporalId :: Integer,
    spatialId :: Integer
  }
  deriving (Eq, Show)

-- AV1 uses the awful scheme of reading bytes from the MSB side
bytesToBits :: [Word8] -> [Bool]
bytesToBits [] = []
bytesToBits (x : xs) = byteToBits x ++ bytesToBits xs
  where
    byteToBits x = [testBit x n | n <- [7, 6 .. 0]]

bitsToBytes :: [Bool] -> [Word8]
bitsToBytes = moreBytes 0 7
  where
    moreBytes :: Word8 -> Int -> [Bool] -> [Word8]
    moreBytes _ 7 [] = []
    moreBytes byte bitIndex [] = [byte]
    moreBytes byte 0 (bit : bits) =
      setBitIf byte 0 bit : moreBytes 0 7 bits
    moreBytes byte bitIndex (bit : bits) =
      moreBytes (setBitIf byte bitIndex bit) (bitIndex - 1) bits
    setBitIf :: Word8 -> Int -> Bool -> Word8
    setBitIf byte _ False = byte
    setBitIf byte bitIndex True = setBit byte bitIndex

decodeFixed :: Integer -> [Bool] -> Maybe (Integer, [Bool])
decodeFixed 0 xs = Just (0, xs)
decodeFixed _ [] = Nothing
decodeFixed n (x : xs) = do
  (y, xsLeft) <- decodeFixed (n - 1) xs
  return (if x then setBit y (fromInteger n - 1) else y, xsLeft)

encodeFixed :: Integer -> Integer -> Maybe [Bool]
encodeFixed 0 0 = Just []
encodeFixed 0 _ = Nothing
encodeFixed n y = do
  let x = testBit y (fromInteger n - 1)
  xs <- encodeFixed (n - 1) (clearBit y (fromInteger n - 1))
  return (x : xs)

integerToObuType :: Integer -> Maybe ObuType
integerToObuType i = [(minBound :: ObuType) ..] `at` i

decodeObuHeader :: [Bool] -> Result (ObuHeader, [Bool])
decodeObuHeader xs = do
  (obuForbiddenBit, xs) <- wrapMaybe "obu_forbidden_bit" $ decodeFixed 1 xs
  if obuForbiddenBit == 0
    then Right ()
    else Left "invalid value of obu_forbidden_bit"
  (obuType, xs) <- wrapMaybe "obu_type" $ decodeFixed 4 xs
  obuType <-
    wrapMaybe "invalid value of obu_type" $ integerToObuType obuType
  (obuExtensionFlag, xs) <- wrapMaybe "obu_extension_flag" $ decodeFixed 1 xs
  (obuHasSizeField, xs) <- wrapMaybe "obu_has_size_field" $ decodeFixed 1 xs
  (obuReserved1Bit, xs) <- wrapMaybe "obu_reserved_1bit" $ decodeFixed 1 xs
  if obuReserved1Bit == 0
    then Right ()
    else Left "invalid value of obu_reserved_1bit"
  if obuExtensionFlag == 1
    then do
      (temporalId, xs) <- wrapMaybe "temporal_id" $ decodeFixed 3 xs
      (spatialId, xs) <- wrapMaybe "spatial_id" $ decodeFixed 2 xs
      (extensionHeaderReserved3Bits, xs) <-
        wrapMaybe "extension_header_reserved_3bits" $
          decodeFixed 3 xs
      if extensionHeaderReserved3Bits == 0
        then Right ()
        else Left "invalid value of extension_header_reserved_3bits"
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

encodeObuHeader :: ObuHeader -> Maybe [Bool]
encodeObuHeader header = do
  let obuForbiddenBit = False
  obuTypeBits <- encodeFixed 4 $ toInteger $ fromEnum $ obuType header
  obuExtensionFlagBits <-
    encodeFixed 1 $ toInteger $ fromEnum $ obuExtensionFlag header
  obuHasSizeFieldBits <-
    encodeFixed 1 $ toInteger $ fromEnum $ obuHasSizeField header
  let obuReserved1Bit = False
  if obuExtensionFlag header
    then do
      temporalIdBits <- encodeFixed 3 $ fromInteger $ temporalId header
      spatialIdBits <- encodeFixed 2 $ fromInteger $ spatialId header
      let extensionHeaderReserved3Bits = [False, False, False]
      return
        ( [obuForbiddenBit]
            ++ obuTypeBits
            ++ obuExtensionFlagBits
            ++ obuHasSizeFieldBits
            ++ [obuReserved1Bit]
            ++ temporalIdBits
            ++ spatialIdBits
            ++ extensionHeaderReserved3Bits
        )
    else
      return
        ( [obuForbiddenBit]
            ++ obuTypeBits
            ++ obuExtensionFlagBits
            ++ obuHasSizeFieldBits
            ++ [obuReserved1Bit]
        )

headerSize :: ObuHeader -> Integer
headerSize header = if obuExtensionFlag header then 2 else 1