import Common (ObuType (..))
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
    encodeFixed, encodeObuHeader,
  )

bytesToBitsTest :: Bool
bytesToBitsTest =
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

bitsToBytesTest :: Bool
bitsToBytesTest =
  and
    [ null $ bitsToBytes [],
      bitsToBytes [True] == [0x80],
      bitsToBytes [True, True, False, False, True, False, False, True]
        == [0xc9],
      bitsToBytes (bytesToBits [0x01, 0x10, 0xff, 0xfa])
        == [0x01, 0x10, 0xff, 0xfa]
    ]

decodeFixedTest =
  and
    [ decodeFixed 0 [] == Just (0, []),
      isNothing $ decodeFixed 1 [],
      decodeFixed 3 [True, True, False, False] == Just (6, [False]),
      decodeFixed 4 [True, True, True, True] == Just (0xf, []),
      decodeFixed 4 [False, False, False, False] == Just (0x0, [])
    ]

encodeFixedTest =
  and
    [ encodeFixed 0 0 == Just [],
      isNothing $ encodeFixed 0 1,
      encodeFixed 3 6 == Just [True, True, False],
      encodeFixed 4 0xf == Just [True, True, True, True],
      encodeFixed 4 0x0 == Just [False, False, False, False]
    ]

testHeader01 =
  ObuHeader
    { obuType = ObuSequenceHeader,
      obuExtensionFlag = False,
      obuHasSizeField = False,
      temporalId = 0,
      spatialId = 0
    }

encodedTestHeader01 =
  [ False, -- obu_forbidden_bit
    False,
    False,
    False,
    True, -- obu_type = OBU_SEQUENCE_HEADER
    False, -- obu_extension_flag
    False, -- obu_has_size_field
    True -- obu_reserved_1bit
  ]

testHeader02 =
  ObuHeader
    { obuType = ObuTileList,
      obuExtensionFlag = True,
      obuHasSizeField = True,
      temporalId = 3,
      spatialId = 1
    }

encodedTestHeader02 =
  [ False, -- obu_forbidden_bit
    True,
    False,
    False,
    False, -- obu_type = OBU_TILE_LIST
    True, -- obu_extension_flag
    True, -- obu_has_size_field
    True, -- obu_reserved_1bit
    False,
    True,
    True, -- temporal_id
    False,
    True, -- spatial_id
    False,
    False,
    False -- extension_header_reserved_3bits
  ]

decodeObuHeaderTest =
  and
    [ decodeObuHeader encodedTestHeader01 == Just (testHeader01, []),
      decodeObuHeader encodedTestHeader02 == Just (testHeader02, [])
    ]

encodeObuHeaderTest =
  and [
    encodeObuHeader testHeader01 == Just encodedTestHeader01,
    encodeObuHeader testHeader02 == Just encodedTestHeader02
  ]

main = do
  putStrLn $ "bytesToBitsTest : " ++ show bytesToBitsTest
  putStrLn $ "bitsToBytesTest : " ++ show bitsToBytesTest
  putStrLn $ "decodeFixedTest : " ++ show decodeFixedTest
  putStrLn $ "encodeFixedTest : " ++ show encodeFixedTest
  putStrLn $ "decodeObuHeaderTest : " ++ show decodeObuHeaderTest
  putStrLn $ "encodeObuHeaderTest : " ++ show encodeObuHeaderTest