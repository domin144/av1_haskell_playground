import CmdArgs
import Common
import Data.List (all)

testPrintTransportFormatArg :: Bool
testPrintTransportFormatArg =
  and
    [ printTransportFormatArg LowOverhead == "low_overhead",
      printTransportFormatArg AnnexB == "annex_b"
    ]

testParseTransportFormatArg :: Bool
testParseTransportFormatArg =
  and
    [ parseTransportFormatArg "low_overhead" == Just LowOverhead,
      parseTransportFormatArg "annex_b" == Just AnnexB
    ]

testParseArgs :: Bool
testParseArgs =
  parseArgs ["input.txt", "output.txt"]
    == Parameters {inputFileName = "input.txt", outputFileName = "output.txt"}

main = do
  putStrLn $ "testPrintTransportFormatArg : " ++ show testPrintTransportFormatArg
  putStrLn $ "testParseTransportFormatArg : " ++ show testParseTransportFormatArg
  putStrLn $ "testParseArgs : " ++ show testParseArgs
