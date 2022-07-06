import CmdArgs
import Common
import Data.List (all)

testPrintTransportFormatArg :: Bool
testPrintTransportFormatArg =
  and
    [ printArg LowOverhead == "low_overhead",
      printArg AnnexB == "annex_b",
      printArg Json == "json"
    ]

testParseTransportFormatArg :: Bool
testParseTransportFormatArg =
  and
    [ parseArg "low_overhead" == Just LowOverhead,
      parseArg "annex_b" == Just AnnexB,
      parseArg "json" == Just Json,
      parseArg "invalid" == (Nothing :: Maybe TransportFormat)
    ]

testParseArgs :: Bool
testParseArgs =
  parseArgs
    [ "-o",
      "output.txt",
      "-i",
      "input.txt",
      "-of",
      "annex_b",
      "-if",
      "low_overhead"
    ]
    == Parameters
      { inputFormat = LowOverhead,
        outputFormat = AnnexB,
        inputFileName = "input.txt",
        outputFileName = "output.txt"
      }

main = do
  putStrLn $ "testPrintTransportFormatArg : " ++ show testPrintTransportFormatArg
  putStrLn $ "testParseTransportFormatArg : " ++ show testParseTransportFormatArg
  putStrLn $ "testParseArgs : " ++ show testParseArgs
