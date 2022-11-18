module JsonTest (jsonTest) where

import Json (decodeBitstream, encodeBitstream)

decodeBitstreamTest :: Bool
decodeBitstreamTest = False

encodeBitstreamTest :: Bool
encodeBitstreamTest = False

jsonTest = do
  putStrLn "JsonTest"
  putStrLn $ "decodeBitstreamTest : " ++ show decodeBitstreamTest
  putStrLn $ "encodeBitstreamTest : " ++ show encodeBitstreamTest