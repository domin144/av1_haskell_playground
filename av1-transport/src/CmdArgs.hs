module CmdArgs
  ( parseArgs,
    TextArg (..),
  )
where

import Common (Parameters (..), Result, TransportFormat (..), wrapMaybe)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (isNothing)

class (Eq a, Enum a, Bounded a) => TextArg a where
  printArg :: a -> String
  parseArg :: String -> Maybe a
  parseArg string = find (\x -> printArg x == string) [minBound ..]

instance TextArg TransportFormat where
  printArg LowOverhead = "low_overhead"
  printArg AnnexB = "annex_b"
  printArg Json = "json"

data ArgId
  = InputFileFormatId
  | InputFileNameId
  | OutputFileFormatId
  | OutputFileNameId
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type ArgMap = Map.Map ArgId String

insertNewOnly :: ArgId -> String -> ArgMap -> Result ArgMap
insertNewOnly argId x argMap =
  if isNothing (Map.lookup argId argMap)
    then Right $ Map.insert argId x argMap
    else Left $ "Parameter " ++ show argId ++ " specified twice"

parseOneArg :: [String] -> Result (ArgId, String, [String])
parseOneArg ("-if" : x : xs) = Right (InputFileFormatId, x, xs)
parseOneArg ("-i" : x : xs) = Right (InputFileNameId, x, xs)
parseOneArg ("-of" : x : xs) = Right (OutputFileFormatId, x, xs)
parseOneArg ("-o" : x : xs) = Right (OutputFileNameId, x, xs)
parseOneArg (x : _) = Left $ "invalid parameter" ++ x
parseOneArg _ = Left "out of parameters"

parseToMap :: [String] -> Result ArgMap
parseToMap = parseMore Map.empty
  where
    parseMore argMap [] = Right argMap
    parseMore argMap xs = do
      (argId, value, ys) <- parseOneArg xs
      extendedMap <- insertNewOnly argId value argMap
      parseMore extendedMap ys

getParameterFromMap :: ArgId -> ArgMap -> Result String
getParameterFromMap argId argMap =
  wrapMaybe ("Missing argument: " ++ show argId) $ Map.lookup argId argMap

parseArgOrWarn :: TextArg a => String -> Result a
parseArgOrWarn string = wrapMaybe "Failed to parse argument" $ parseArg string

parseArgs :: [String] -> Result Parameters
parseArgs ["--help"] = Left "Help requested!"
parseArgs xs = do
  argMap <- parseToMap xs
  inputFormatValue <-
    getParameterFromMap InputFileFormatId argMap >>= parseArgOrWarn
  inputFileNameValue <- getParameterFromMap InputFileNameId argMap
  outputFormatValue <-
    getParameterFromMap OutputFileFormatId argMap >>= parseArgOrWarn
  outputFileNameValue <- getParameterFromMap OutputFileNameId argMap
  return
    Parameters
      { inputFormat = inputFormatValue,
        inputFileName = inputFileNameValue,
        outputFormat = outputFormatValue,
        outputFileName = outputFileNameValue
      }
