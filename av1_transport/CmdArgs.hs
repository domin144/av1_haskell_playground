module CmdArgs
  ( parseArgs,
    printTransportFormatArg,
    parseTransportFormatArg,
    TextArg (..),
  )
where

import Common
import Data.List(find)

class (Eq a, Enum a, Bounded a) => TextArg a where
  printArg :: a -> String
  parseArg :: String -> Maybe a
  parseArg string = find (\x -> printArg x == string) [minBound ..]

instance TextArg TransportFormat where
  printArg LowOverhead = "low_overhead"
  printArg AnnexB = "annex_b"

printTransportFormatArg :: TransportFormat -> String
printTransportFormatArg = printArg

parseTransportFormatArg :: String -> Maybe TransportFormat
parseTransportFormatArg = parseArg

parseArgs :: [String] -> Parameters
parseArgs [input, output] =
  Parameters {inputFileName = input, outputFileName = output}
parseArgs _ = error "invalid number of arguments"
