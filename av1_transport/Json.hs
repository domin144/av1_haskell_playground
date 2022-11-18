module Json
  ( decodeBitstream,
    encodeBitstream,
  )
where

import Common
  ( ObuBytes,
    Result,
    wrapMaybe,
    wrapResult,
  )
import Data.Word (Word8)
import ObuHeader
  ( ObuHeader (..),
    bitsToBytes,
    bytesToBits,
    decodeObuHeader,
    encodeObuHeader,
    headerSize,
  )

decodeBitstream :: [Word8] -> Result [ObuBytes]
decodeBitstream = error "TODO"

encodeBitstream :: [ObuBytes] -> Maybe [Word8]
encodeBitstream = error "TODO"