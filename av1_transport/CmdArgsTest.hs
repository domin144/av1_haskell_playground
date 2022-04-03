import CmdArgs
import Common
import Data.List (all)

testPrintTransportFormatArg :: Bool
testPrintTransportFormatArg =
  and
    [ printTransportFormatArg LowOverhead == "low_overhead",
      printTransportFormatArg AnnexB == "annex_b"
    ]

testParseArgs0 :: Bool
testParseArgs0 =
  parseArgs ["input.txt", "output.txt"]
    == Parameters {inputFileName = "input.txt", outputFileName = "output.txt"}

main = do
  putStrLn $ "testParseArgs0 : " ++ show testParseArgs0
  putStrLn $ "testPrintTransportFormatArg : " ++ show testParseArgs0
