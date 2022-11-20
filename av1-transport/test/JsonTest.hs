module JsonTest (jsonTest) where

import qualified Codec.Binary.UTF8.String as UTF8
import Json (decodeBitstream, encodeBitstream)

decodeBitstreamTest :: Bool
decodeBitstreamTest = False

encodeBitstreamTest :: Bool
encodeBitstreamTest = False

utf8Test :: Bool
utf8Test =
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

jsonTest :: IO ()
jsonTest = do
  putStrLn "JsonTest"
  putStrLn $ "decodeBitstreamTest : " ++ show decodeBitstreamTest
  putStrLn $ "encodeBitstreamTest : " ++ show encodeBitstreamTest
  putStrLn $ "utf8Test: " ++ show utf8Test