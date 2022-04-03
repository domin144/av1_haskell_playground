module CmdArgs (parseArgs, printTransportFormatArg, parseTransportFormatArg) where

import Common

class TextArg a where
  printArg :: a -> String
  parseArg :: String -> a

instance TextArg TransportFormat where
  printArg LowOverhead = "low_overhead"
  printArg AnnexB = "annex_b"
  parseArg "low_overhead" = LowOverhead
  parseArg "annex_b" = AnnexB
  parseArg _ = error "invalid transport format"

printTransportFormatArg :: TransportFormat -> String
printTransportFormatArg = printArg

parseTransportFormatArg :: String -> TransportFormat
parseTransportFormatArg = parseArg

parseArgs :: [String] -> Parameters
parseArgs [input, output] =
  Parameters {inputFileName = input, outputFileName = output}
parseArgs _ = error "invalid number of arguments"
