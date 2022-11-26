module ObuHeaderTest (obuHeaderTest) where

import Common (ObuType (ObuSequenceHeader, ObuTileList))
import Data.Maybe (isNothing)
import ObuHeader
  ( ObuHeader
      ( ObuHeader,
        obuExtensionFlag,
        obuHasSizeField,
        obuType,
        spatialId,
        temporalId
      ),
    bitsToBytes,
    bytesToBits,
    decodeFixed,
    decodeObuHeader,
    encodeFixed,
    encodeObuHeader,
  )
import TestTree (TestTree (Test, TestSet))

bytesToBitsTest :: TestTree
bytesToBitsTest =
  TestSet
    "bytesToBitsTest"
    [ Test "[0xf5, 0x3]" $
        bytesToBits [0xf5, 0x3]
          == [ True, -- 0
               True,
               True,
               True,
               False, -- 4
               True,
               False,
               True,
               False, -- 8
               False,
               False,
               False,
               False, -- 12
               False,
               True,
               True
             ]
    ]

bitsToBytesTest :: TestTree
bitsToBytesTest =
  TestSet
    "bitsToBytesTest"
    [ Test "null" $ null $ bitsToBytes [],
      Test "one bit" $ bitsToBytes [True] == [0x80],
      Test "two bytes" $
        bitsToBytes [True, True, False, False, True, False, False, True]
          == [0xc9],
      Test "four bytes" $
        bitsToBytes (bytesToBits [0x01, 0x10, 0xff, 0xfa])
          == [0x01, 0x10, 0xff, 0xfa]
    ]

decodeFixedTest :: TestTree
decodeFixedTest =
  TestSet
    "decodeFixedTest"
    [ Test "0 bits" $ decodeFixed 0 [] == Just (0, []),
      Test "overrun" $ isNothing $ decodeFixed 1 [],
      Test "6 in 3 bits" $ decodeFixed 3 [True, True, False, False] == Just (6, [False]),
      Test "0xf in 4 bits" $ decodeFixed 4 [True, True, True, True] == Just (0xf, []),
      Test "0x0 in 4 bits" $ decodeFixed 4 [False, False, False, False] == Just (0x0, [])
    ]

encodeFixedTest :: TestTree
encodeFixedTest =
  TestSet
    "encodeFixedTest"
    [ Test "0 bits" $ encodeFixed 0 0 == Just [],
      Test "overflow" $ isNothing $ encodeFixed 0 1,
      Test "6 in 3 bits" $ encodeFixed 3 6 == Just [True, True, False],
      Test "0xf in 4 bits" $ encodeFixed 4 0xf == Just [True, True, True, True],
      Test "0x0 in 4 bits" $
        encodeFixed 4 0x0 == Just [False, False, False, False]
    ]

testHeader01 :: ObuHeader
testHeader01 =
  ObuHeader
    { obuType = ObuSequenceHeader,
      obuExtensionFlag = False,
      obuHasSizeField = False,
      temporalId = 0,
      spatialId = 0
    }

encodedTestHeader01 :: [Bool]
encodedTestHeader01 =
  [ False, -- obu_forbidden_bit
    False,
    False,
    False,
    True, -- obu_type = OBU_SEQUENCE_HEADER
    False, -- obu_extension_flag
    False, -- obu_has_size_field
    False -- obu_reserved_1bit
  ]

testHeader02 :: ObuHeader
testHeader02 =
  ObuHeader
    { obuType = ObuTileList,
      obuExtensionFlag = True,
      obuHasSizeField = True,
      temporalId = 3,
      spatialId = 1
    }

encodedTestHeader02 :: [Bool]
encodedTestHeader02 =
  [ False, -- obu_forbidden_bit
    True,
    False,
    False,
    False, -- obu_type = OBU_TILE_LIST
    True, -- obu_extension_flag
    True, -- obu_has_size_field
    False, -- obu_reserved_1bit
    False,
    True,
    True, -- temporal_id
    False,
    True, -- spatial_id
    False,
    False,
    False -- extension_header_reserved_3bits
  ]

decodeObuHeaderTest :: TestTree
decodeObuHeaderTest =
  TestSet
    "decodeObuHeaderTest"
    [ Test "case 01" $
        decodeObuHeader encodedTestHeader01 == Right (testHeader01, []),
      Test "case 02" $
        decodeObuHeader encodedTestHeader02 == Right (testHeader02, [])
    ]

encodeObuHeaderTest :: TestTree
encodeObuHeaderTest =
  TestSet
    "encodeObuHeaderTest"
    [ Test "case 01" $
        encodeObuHeader testHeader01 == Just encodedTestHeader01,
      Test "case 02" $
        encodeObuHeader testHeader02 == Just encodedTestHeader02
    ]

obuHeaderTest :: TestTree
obuHeaderTest =
  TestSet
    "obuHeaderTest"
    [ bytesToBitsTest,
      bitsToBytesTest,
      decodeFixedTest,
      encodeFixedTest,
      decodeObuHeaderTest,
      encodeObuHeaderTest
    ]
