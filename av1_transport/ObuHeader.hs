module ObuHeader (bytesToBits, decodeFixed) where

import Common (ObuBytes, ObuType)
import Data.Bits (setBit, testBit)
import Data.Word (Word8)
import GHC.IO.Exception (IOErrorType (NoSuchThing))

data ObuHeader = ObuHeader
  { obuType :: ObuType,
    obuExtensionFlag :: Bool,
    obuHasSizeField :: Bool,
    temporalId :: Integer,
    spatialId :: Integer
  }

-- AV1 uses the awful scheme of reading bytes from the MSB side
bytesToBits :: [Word8] -> [Bool]
bytesToBits [] = []
bytesToBits (x : xs) = byteToBits x ++ bytesToBits xs
  where
    byteToBits x = [testBit x n | n <- [7, 6 .. 0]]

decodeFixed :: Integer -> [Bool] -> Maybe (Integer, [Bool])
decodeFixed 0 xs = Just (0, xs)
decodeFixed _ [] = Nothing
decodeFixed n (x : xs) = case decodeFixed (n - 1) xs of
  Nothing -> Nothing
  Just (y, xsLeft) ->
    Just (if x then setBit y (fromInteger n - 1) else y, xsLeft)

-- integerToObuType :: Integer -> Maybe ObuType
-- integerToObuType i = [(minBound :: ObuType) ..] `at` i

integerToObuType :: Integer -> ObuType
integerToObuType i = [(minBound :: ObuType) ..] !! fromIntegral i

decodeObuHeader :: [Bool] -> Maybe (ObuHeader, [Bool])
decodeObuHeader xs =
  case decodeFixed 1 xs of
    Nothing -> Nothing
    Just (1, _) -> Nothing
    Just (0, xs2) ->
      case decodeFixed 2 xs2 of
        Nothing -> Nothing
        Just (obuType, xs3) ->
          case decodeFixed 1 xs3 of
            Nothing -> Nothing
            Just (obuExtensionFlag, xs4) ->
              case decodeFixed 1 xs4 of
                Nothing -> Nothing
                Just (obuHasSizeField, xs5) ->
                  case decodeFixed 1 xs5 of
                    Nothing -> Nothing
                    Just (1, _) -> Nothing
                    Just (0, xs6) ->
                      if obuExtensionFlag == 1
                        then case decodeFixed 3 xs6 of
                          Nothing -> Nothing
                          Just (temporalId, xs7) ->
                            case decodeFixed 2 xs7 of
                              Nothing -> Nothing
                              Just (spatialId, xs8) ->
                                case decodeFixed 3 xs8 of
                                  Nothing -> Nothing
                                  Just (0, xs9) ->
                                    Just
                                      ( ObuHeader
                                          { obuType = integerToObuType obuType,
                                            obuExtensionFlag = obuExtensionFlag == 1,
                                            obuHasSizeField = obuHasSizeField == 1,
                                            temporalId = temporalId,
                                            spatialId = spatialId
                                          },
                                        xs9
                                      )
                                  Just (_, _) -> Nothing
                        else
                          Just
                            ( ObuHeader
                                { obuType = integerToObuType obuType,
                                  obuExtensionFlag = obuExtensionFlag == 1,
                                  obuHasSizeField = obuHasSizeField == 1,
                                  temporalId = 0,
                                  spatialId = 0
                                },
                              xs6
                            )