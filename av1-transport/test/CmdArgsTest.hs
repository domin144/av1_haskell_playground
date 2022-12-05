module CmdArgsTest (cmdArgsTest) where

import CmdArgs (TextArg (parseArg, printArg), parseArgs)
import Common
  ( Parameters
      ( Parameters,
        inputFileName,
        inputFormat,
        outputFileName,
        outputFormat
      ),
    TransportFormat (..),
  )
import TestTree (TestTree (Test, TestSet))

testPrintTransportFormatArg :: TestTree
testPrintTransportFormatArg =
  TestSet
    "testPrintTransportFormatArg"
    [ Test "low_overhead" $ printArg LowOverhead == "low_overhead",
      Test "annex_b" $ printArg AnnexB == "annex_b",
      Test "json" $ printArg Json == "json"
    ]

testParseTransportFormatArg :: TestTree
testParseTransportFormatArg =
  TestSet
    "testParseTransportFormatArg"
    [ Test "low_overhead" $ parseArg "low_overhead" == Just LowOverhead,
      Test "annex_b" $ parseArg "annex_b" == Just AnnexB,
      Test "json" $ parseArg "json" == Just Json,
      Test "invalid" $ parseArg "invalid" == (Nothing :: Maybe TransportFormat)
    ]

testParseArgs :: TestTree
testParseArgs =
  Test "testParseArgs" $
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
      == Right
        ( Parameters
            { inputFormat = LowOverhead,
              outputFormat = AnnexB,
              inputFileName = "input.txt",
              outputFileName = "output.txt"
            }
        )

cmdArgsTest :: TestTree
cmdArgsTest =
  TestSet
    "cmdArgsTest"
    [ testPrintTransportFormatArg,
      testParseTransportFormatArg,
      testParseArgs
    ]
