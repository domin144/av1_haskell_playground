module LowOverhead
  ( decodeBitstream,
    encodeBitstream,
  )
where

import Common (ObuBytes, decodeLeb128, encodeLeb128)
import Data.List (concat)
import Data.Word (Word8)
import ObuHeader
  ( ObuHeader (..),
    bitsToBytes,
    bytesToBits,
    decodeObuHeader,
    encodeObuHeader,
  )

maybeSplitAt :: Integer -> [a] -> Maybe ([a], [a])
maybeSplitAt 0 xs = Just ([], xs)
maybeSplitAt n [] = Nothing
maybeSplitAt n (x : xs)
  | n > 0 = do
    (left, right) <- maybeSplitAt (n - 1) xs
    return (x : left, right)
  | otherwise = Nothing

decodeHeader :: [Word8] -> Maybe (ObuHeader, [Word8])
decodeHeader bytes = do
  let bits = bytesToBits bytes
  (header, bits) <- decodeObuHeader bits
  return (header, bitsToBytes bits)

encodeHeader :: ObuHeader -> Maybe [Word8]
encodeHeader header = do
  encodedHeader <- encodeObuHeader header
  return (bitsToBytes encodedHeader)

decodeSize :: ObuHeader -> [Word8] -> Maybe (Integer, Integer, [Word8])
decodeSize header bytes =
  if obuHasSizeField header
    then decodeLeb128 bytes
    else Nothing

decodeBitstream :: [Word8] -> Maybe [ObuBytes]
decodeBitstream bytes = do
  (header, bytesPastHeader) <- decodeHeader bytes
  (obuSize, obuSizeSize, bytesPastSize) <- decodeSize header bytes
  let headerSize = if obuExtensionFlag header then 2 else 1
  (obu, bytes) <- maybeSplitAt (headerSize + obuSizeSize + obuSize) bytes
  obus <- decodeBitstream bytes
  return (obu : obus)

encodeBitstream :: [ObuBytes] -> Maybe [Word8]
encodeBitstream obus = do
  encodedObus <- mapM encodeObu obus
  return (concat encodedObus)
  where
    encodeObu :: ObuBytes -> Maybe [Word8]
    encodeObu bytes = do
      (header, bytesPastHeader) <- decodeHeader bytes
      if obuHasSizeField header
        then return bytes
        else do
          let header = header {obuHasSizeField = True}
          let headerSize = if obuExtensionFlag header then 2 else 1
          let obuSize = toInteger $ length bytesPastHeader
          encodedHeader <- encodeHeader header
          (encodedSize, encodedSizeSize) <- encodeLeb128 obuSize
          return (encodedHeader ++ encodedSize ++ bytesPastHeader)