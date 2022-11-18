module Main (main) where

import qualified AnnexB
import CmdArgs (parseArgs)
import Common
  ( Parameters (..),
    Result,
    TransportFormat (..),
    wrapMaybe,
    wrapResult,
  )
import qualified Data.ByteString.Lazy as B
import Data.List (concat)
import Data.Word (Word8)
import qualified LowOverhead
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openFile)

transform :: TransportFormat -> TransportFormat -> [Word8] -> Result [Word8]
transform inputFormat outputFormat input = do
  obus <- wrapResult "decode" $ case inputFormat of
    AnnexB -> wrapMaybe "annex B" $ AnnexB.decodeBitstream input
    LowOverhead -> wrapResult "low overhead" $ LowOverhead.decodeBitstream input
    Json -> Left "JSON input not implemented yet"
  wrapResult "encode: " $ case outputFormat of
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
    Left error -> putStrLn $ "Failed to convert file: " ++ error
  hClose inputHandle
  hClose outputHandle

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = parseArgs args
  process parsedArgs
