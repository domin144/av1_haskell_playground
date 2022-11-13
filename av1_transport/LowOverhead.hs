module LowOverhead
  ( decodeBitstream,
    encodeBitstream,
  )
where

import Common
  ( ObuBytes,
    Result,
    decodeLeb128,
    encodeLeb128,
    wrapMaybe,
    wrapResult,
  )
import Data.List (concat)
import Data.Word (Word8)
import Distribution.Simple.Utils (xargs)
import ObuHeader
  ( ObuHeader (..),
    bitsToBytes,
    bytesToBits,
    decodeObuHeader,
    encodeObuHeader,
    headerSize,
  )

maybeSplitAt :: Integer -> [a] -> Maybe ([a], [a])
maybeSplitAt 0 xs = Just ([], xs)
maybeSplitAt n [] = Nothing
maybeSplitAt n (x : xs)
  | n > 0 = do
    (left, right) <- maybeSplitAt (n - 1) xs
    return (x : left, right)
  | otherwise = Nothing

decodeHeader :: [Word8] -> Result (ObuHeader, [Word8])
decodeHeader bytes = do
  (header, _) <- decodeObuHeader $ bytesToBits bytes
  (_, bytes) <- wrapMaybe "split" $ maybeSplitAt (headerSize header) bytes
  return (header, bytes)

encodeHeader :: ObuHeader -> Maybe [Word8]
encodeHeader header = do
  encodedHeader <- encodeObuHeader header
  return (bitsToBytes encodedHeader)

decodeSize :: ObuHeader -> [Word8] -> Maybe (Integer, Integer, [Word8])
decodeSize header bytes =
  if obuHasSizeField header
    then decodeLeb128 bytes
    else Nothing

decodeBitstream :: [Word8] -> Result [ObuBytes]
decodeBitstream [] = Right []
decodeBitstream bytes = do
  (header, bytesPastHeader) <- wrapResult "header" $ decodeHeader bytes
  (obuSize, obuSizeSize, bytesPastSize) <-
    wrapMaybe "size" $ decodeSize header bytesPastHeader
  let headerSize = ObuHeader.headerSize header
  (obu, bytes) <-
    wrapMaybe "split" $
      maybeSplitAt (headerSize + obuSizeSize + obuSize) bytes
  obus <- decodeBitstream bytes
  return (obu : obus)

encodeBitstream :: [ObuBytes] -> Maybe [Word8]
encodeBitstream obus = do
  encodedObus <- mapM encodeObu obus
  return (concat encodedObus)
  where
    encodeObu :: ObuBytes -> Maybe [Word8]
    encodeObu bytes = do
      (header, bytesPastHeader) <- case decodeHeader bytes of
        Right x -> Just x
        Left _ -> Nothing
      if obuHasSizeField header
        then return bytes
        else do
          header <- return $ header {obuHasSizeField = True}
          let obuSize = toInteger $ length bytesPastHeader
          encodedHeader <- encodeHeader header
          (encodedSize, encodedSizeSize) <- encodeLeb128 obuSize
          return (encodedHeader ++ encodedSize ++ bytesPastHeader)