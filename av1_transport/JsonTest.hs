import Json (decodeBitstream, encodeBitstream)

decodeBitstreamTest :: Bool
decodeBitstreamTest = False

encodeBitstreamTest :: Bool
encodeBitstreamTest = False

main = do
  putStrLn "JsonTest"
  putStrLn $ "decodeBitstreamTest : " ++ show decodeBitstreamTest
  putStrLn $ "encodeBitstreamTest : " ++ show encodeBitstreamTest