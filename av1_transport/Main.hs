import qualified AnnexB
import CmdArgs (parseArgs)
import Common (Parameters (..), TransportFormat (..))
import qualified Data.ByteString.Lazy as B
import Data.List (concat)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hClose, openFile)

transform :: TransportFormat -> TransportFormat -> [Word8] -> [Word8]
transform AnnexB AnnexB input = concat obus
  where
    Just structuredObus = AnnexB.decodeBitstream input
    obus = AnnexB.flattenTheBitstream structuredObus
transform _ _ _ = error "not implemented yet"

process :: Parameters -> IO ()
process parsedArgs = do
  putStrLn $ "input: " ++ inputFileName parsedArgs
  putStrLn $ "output: " ++ outputFileName parsedArgs
  inputHandle <- openFile (inputFileName parsedArgs) ReadMode
  outputHandle <- openFile (outputFileName parsedArgs) WriteMode
  input <- B.hGetContents inputHandle
  let output = transform (inputFormat parsedArgs) (outputFormat parsedArgs) (B.unpack input)
  B.hPutStr outputHandle (B.pack output)
  hClose inputHandle
  hClose outputHandle

main = do
  args <- getArgs
  let parsedArgs = parseArgs args
  process parsedArgs
