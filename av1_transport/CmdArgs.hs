module CmdArgs (parseArgs) where

import Common

printTransportFormatArg :: TransportFormat -> String
printTransportFormatArg LowOverhead = "low_overhead"
printTransportFormatArg AnnexB = "annex_b"

parseTransportFormatArg :: String -> TransportFormat
parseTransportFormatArg "low_overhead" = LowOverhead
parseTransportFormatArg "annex_b" = AnnexB
parseTransportFormatArg _ = error "invalid transport format"

parseArgs :: [String] -> Parameters
parseArgs [input, output] =
  Parameters {inputFileName = input, outputFileName = output}
parseArgs _ = error "invalid number of arguments"
