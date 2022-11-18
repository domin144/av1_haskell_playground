module BytestreamLayer
  ( FrameUnit,
    TemporalUnit,
    TypedObu,
  )
where

import Common
  ( ObuBytes,
    ObuType (..),
  )
import Data.Word (Word8)

type FrameUnit = [ObuBytes]

type TemporalUnit = [FrameUnit]

type TypedObu = (ObuType, ObuBytes)
