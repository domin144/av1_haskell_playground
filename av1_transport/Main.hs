import qualified AnnexB
import CmdArgs (parseArgs)
import Common (Parameters (..), TransportFormat (..))
import qualified Data.ByteString.Lazy as B
import Data.List (concat)
import Data.Word (Word8)
import qualified LowOverhead
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openFile)

transform :: TransportFormat -> TransportFormat -> [Word8] -> Maybe [Word8]
transform inputFormat outputFormat input = do
  obus <- case inputFormat of
    AnnexB -> do
      structuredObus <- AnnexB.decodeBitstream input
      return $ AnnexB.flattenTheBitstream structuredObus
    LowOverhead -> LowOverhead.decodeBitstream input
    Json -> Nothing
  case outputFormat of
    AnnexB -> do
      structuredObus <- AnnexB.groupTheBistream obus
      AnnexB.encodeBitstream structuredObus
    LowOverhead -> LowOverhead.encodeBitstream obus
    Json -> Nothing

process :: Parameters -> IO ()
process parsedArgs = do
  putStrLn $ "input: " ++ inputFileName parsedArgs
  putStrLn $ "output: " ++ outputFileName parsedArgs
  inputHandle <- openFile (inputFileName parsedArgs) ReadMode
  outputHandle <- openFile (outputFileName parsedArgs) WriteMode
  input <- B.hGetContents inputHandle
  let maybeOutput = transform (inputFormat parsedArgs) (outputFormat parsedArgs) (B.unpack input)
  case maybeOutput of
    Just output -> B.hPutStr outputHandle (B.pack output)
    Nothing -> putStrLn "Failed to convert file"
  hClose inputHandle
  hClose outputHandle

main = do
  args <- getArgs
  let parsedArgs = parseArgs args
  process parsedArgs
