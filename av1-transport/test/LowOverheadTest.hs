module LowOverheadTest (lowOverheadTest) where

import Common (ObuBytes)
import LowOverhead (decodeBitstream, encodeBitstream)
import TestTree (TestTree (Test, TestSet))

obu01 :: ObuBytes
obu01 =
  [ 0x12, -- temporal delimiter
    0x00 -- size = 0
  ]

obu02 :: ObuBytes
obu02 =
  [ 0x0a, -- obu sequence header
    0x0b, -- size = 11
    0x00,
    0x00,
    0x00,
    0x24,
    0xcf,
    0x7f,
    0x0d,
    0xbf,
    0xff,
    0x30,
    0x08
  ]

obu01NoSize :: ObuBytes
obu01NoSize =
  [ 0x10 -- temporal delimiter
  ]

obu02NoSize :: ObuBytes
obu02NoSize =
  [ 0x08, -- obu sequence header
    0x00,
    0x00,
    0x00,
    0x24,
    0xcf,
    0x7f,
    0x0d,
    0xbf,
    0xff,
    0x30,
    0x08
  ]

decodeBitstreamTest :: TestTree
decodeBitstreamTest =
  Test "decodeBitstreamTest" $
    decodeBitstream (obu01 ++ obu02) == Right [obu01, obu02]

encodeBitstreamTest :: TestTree
encodeBitstreamTest =
  Test "encodeBitstreamTest" $
    and
      [ encodeBitstream [obu01, obu02] == Just (obu01 ++ obu02),
        encodeBitstream [obu01NoSize, obu02NoSize] == Just (obu01 ++ obu02)
      ]

lowOverheadTest :: TestTree
lowOverheadTest =
  TestSet
    "lowOverheadTest"
    [decodeBitstreamTest, encodeBitstreamTest]
