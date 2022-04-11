module CmdArgs
  ( parseArgs,
    TextArg (..),
  )
where

import Common
import Data.List(find)
import Data.Maybe(isNothing)
import qualified Data.Map as Map

class (Eq a, Enum a, Bounded a) => TextArg a where
  printArg :: a -> String
  parseArg :: String -> Maybe a
  parseArg string = find (\x -> printArg x == string) [minBound ..]

instance TextArg TransportFormat where
  printArg LowOverhead = "low_overhead"
  printArg AnnexB = "annex_b"

data ArgId
  = InputFileFormatId
  | InputFileNameId
  | OutputFileFormatId
  | OutputFileNameId
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type ArgMap = Map.Map ArgId String

insertNewOnly :: ArgId -> String -> ArgMap -> ArgMap
insertNewOnly argId x map =
  if isNothing (Map.lookup argId map)
    then Map.insert argId x map
    else error ("Parameter " ++ show argId ++ " specified twice")

parseOneArg :: [String] -> (ArgId, String, [String])
parseOneArg ("-if" : x : xs) = (InputFileFormatId, x, xs)
parseOneArg ("-i" : x : xs) = (InputFileNameId, x, xs)
parseOneArg ("-of" : x : xs) = (OutputFileFormatId, x, xs)
parseOneArg ("-o" : x : xs) = (OutputFileNameId, x, xs)
parseOneArg _ = error "invalid arguments"

parseToMap :: ArgMap -> [String] -> ArgMap
parseToMap argMap [] = argMap
parseToMap argMap xs =
  parseToMap (insertNewOnly argId value argMap) ys
  where
    (argId, value, ys) = parseOneArg xs

mapElement :: Ord key => key -> Map.Map key value -> value
mapElement key map = case Map.lookup key map of
  Just x -> x
  Nothing -> error "missing element in a map"

parseArgOrDie :: TextArg a => String -> a
parseArgOrDie string = case parseArg string of
  Just x -> x
  Nothing -> error "could not parse arg"

parseArgs :: [String] -> Parameters
parseArgs xs =
  let map = parseToMap Map.empty xs
   in Parameters
        { inputFormat = parseArgOrDie $ mapElement InputFileFormatId map,
          inputFileName = mapElement InputFileNameId map,
          outputFormat = parseArgOrDie $ mapElement OutputFileFormatId map,
          outputFileName = mapElement OutputFileNameId map
        }
