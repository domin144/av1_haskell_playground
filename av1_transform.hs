import System.Environment as Env
import System.IO

data TransportFormat = LowOverhead | AnnexB deriving (Eq, Ord, Show, Read, Bounded, Enum)

printTransportFormatArg :: TransportFormat -> String
printTransportFormatArg LowOverhead = "low_overhead"
printTransportFormatArg AnnexB = "annex_b"

parseTransportFormatArg :: String -> TransportFormat
parseTransportFormatArg "low_overhead" = LowOverhead
parseTransportFormatArg "annex_b" = AnnexB
parseTransportFormatArg _ = error "invalid transport format"

data Parameters = Parameters
  {
    -- inputFormat :: TransportFormat,
    inputFileName :: String,
    -- outoputFormat :: TransportFormat,
    outputFileName :: String
  }

main = do
  args <- Env.getArgs
  let parsedArgs = parseArgs args
  putStrLn $ "input: " ++ inputFileName parsedArgs
  putStrLn $ "output: " ++ outputFileName parsedArgs
  inputHandle <- openFile (inputFileName parsedArgs) ReadMode
  outputHandle <- openFile (outputFileName parsedArgs) WriteMode
  contents <- hGetContents inputHandle
  hPutStr outputHandle contents
  hClose inputHandle
  hClose outputHandle

parseArgs :: [String] -> Parameters
parseArgs [input, output] =
  Parameters {inputFileName = input, outputFileName = output}
parseArgs _ = error "invalid number of arguments"
