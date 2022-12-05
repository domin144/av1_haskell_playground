module Main (main) where

import qualified AnnexB
import CmdArgs (parseArgs)
import Common
  ( Parameters (inputFileName, outputFileName, inputFormat, outputFormat),
    Result,
    TransportFormat (AnnexB, LowOverhead, Json),
    wrapMaybe,
    wrapResult,
  )
import qualified Data.ByteString.Lazy as B
import Data.Word (Word8)
import qualified LowOverhead
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openFile)

transform :: TransportFormat -> TransportFormat -> [Word8] -> Result [Word8]
transform inputFormatArg outputFormatArg input = do
  obus <- wrapResult "decode" $ case inputFormatArg of
    AnnexB -> wrapMaybe "annex B" $ AnnexB.decodeBitstream input
    LowOverhead -> wrapResult "low overhead" $ LowOverhead.decodeBitstream input
    Json -> Left "JSON input not implemented yet"
  wrapResult "encode: " $ case outputFormatArg of
    AnnexB -> wrapMaybe "annex B" $ AnnexB.encodeBitstream obus
    LowOverhead -> wrapMaybe "low overhead" $ LowOverhead.encodeBitstream obus
    Json -> Left "JSON output not implemented yet"

process :: Parameters -> IO ()
process parsedArgs = do
  putStrLn $ "input: " ++ inputFileName parsedArgs
  putStrLn $ "output: " ++ outputFileName parsedArgs
  inputHandle <- openFile (inputFileName parsedArgs) ReadMode
  outputHandle <- openFile (outputFileName parsedArgs) WriteMode
  input <- B.hGetContents inputHandle
  let maybeOutput = transform (inputFormat parsedArgs) (outputFormat parsedArgs) (B.unpack input)
  case maybeOutput of
    Right output -> B.hPutStr outputHandle (B.pack output)
    Left errorMessage -> putStrLn $ "Failed to convert file: " ++ errorMessage
  hClose inputHandle
  hClose outputHandle

helpString :: String
helpString =
  "AV-1 bytestream format converter\n\
  \\n\
  \Usage:\n\
  \    av1-transport-exe -if INPUT_FORMAT -i INPUT_FILE -of OUTPUT_FORMAT -o OUTPUT_FILE\n\
  \\n\
  \    INPUT_FORMAT and OUTPUT_FORMAT are one of \"annex_b\", \"low_overhead\" or \"json\"\n\
  \"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left errorMessage -> do
      putStrLn $ "Failed to parse arguments: " ++ errorMessage
      putStr helpString
    Right parsedArgs -> process parsedArgs
