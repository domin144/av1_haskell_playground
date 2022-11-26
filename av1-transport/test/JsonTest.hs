module JsonTest (jsonTest) where

import qualified Codec.Binary.UTF8.String as UTF8
import Common (ObuBytes, Result)
import Data.Word (Word8)
import Json (decodeBitstream, encodeBitstream)
import TestTree (TestTree (Test, TestSet))

utf8Test :: TestTree
utf8Test =
  Test "utf8Test" $
    UTF8.encode "Dominik Wójt"
      == [ 0x44,
           0x6f,
           0x6d,
           0x69,
           0x6e,
           0x69,
           0x6b,
           0x20,
           0x57,
           0xc3,
           0xb3,
           0x6a,
           0x74
         ]

obus :: [ObuBytes]
obus =
  [[18, 0], [10, 11, 0, 0, 0, 36, 207, 127, 13, 191, 255, 48, 8]]

obusInPrettyJson :: [Word8]
obusInPrettyJson =
  UTF8.encode
    "\
    \{\n\
    \  \"obus\" : [\n\
    \    {\n\
    \      \"data\" : [18, 0]\n\
    \    },\n\
    \    {\n\
    \      \"data\" : [10, 11, 0, 0, 0, 36, 207, 127, 13, 191, 255, 48, 8]\n\
    \    }\n\
    \  ]\n\
    \}\n\
    \"

decodeBitstreamTest :: TestTree
decodeBitstreamTest =
  Test "decodeBitstreamTest" $
    decodeBitstream obusInPrettyJson == Right obus

encodeBitstreamTest :: TestTree
encodeBitstreamTest = Test "encodeBitstreamTest" $
  case result of
    Left _ -> False
    Right decodedObus -> decodedObus == obus
  where
    result :: Result [ObuBytes]
    result = do
      encodedObus <- encodeBitstream obus
      decodeBitstream encodedObus

jsonTest :: TestTree
jsonTest =
  TestSet
    "JsonTest"
    [ utf8Test,
      decodeBitstreamTest,
      encodeBitstreamTest
    ]
