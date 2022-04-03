import System.Environment as Env
import System.IO

import Common
import CmdArgs(parseArgs)

process :: Parameters -> IO ()
process parsedArgs = do
  putStrLn $ "input: " ++ inputFileName parsedArgs
  putStrLn $ "output: " ++ outputFileName parsedArgs
  inputHandle <- openFile (inputFileName parsedArgs) ReadMode
  outputHandle <- openFile (outputFileName parsedArgs) WriteMode
  contents <- hGetContents inputHandle
  hPutStr outputHandle contents
  hClose inputHandle
  hClose outputHandle

main = do
  args <- Env.getArgs
  let parsedArgs = parseArgs args
  process parsedArgs
