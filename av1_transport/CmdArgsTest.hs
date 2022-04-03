import CmdArgs
import Common

test0 :: Bool
test0 =
  parseArgs ["input.txt", "output.txt"]
    == Parameters {inputFileName = "input.txt", outputFileName = "output.txt"}

main = do
    putStrLn $ "Test 0 : " ++ show test0