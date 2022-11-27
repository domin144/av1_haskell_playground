{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Json
  ( decodeBitstream,
    encodeBitstream,
  )
where

import Common (ObuBytes, Result, wrapMaybe)
import Data.Aeson
  ( FromJSON,
    Object,
    ToJSON,
    Value,
    decode,
    encode,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (pack, unpack)
import Data.Word (Word8)

newtype Bitstream = Bitstream {getBitstream :: [ObuBytes]}

instance FromJSON Bitstream where
  parseJSON :: Value -> Parser Bitstream
  parseJSON = withObject "Bitstream" $ \bitstreamObject ->
    do
      obuObjects <- bitstreamObject .: "obus" :: Parser [Object]
      obus <- mapM parseObuJSON obuObjects
      return $ Bitstream obus
    where
      parseObuJSON :: Object -> Parser ObuBytes
      parseObuJSON obuObject = obuObject .: "data"

instance ToJSON Bitstream where
  toJSON :: Bitstream -> Value
  toJSON (Bitstream obus) = object [("obus", toJSON $ map obuToJSON obus)]
    where
      obuToJSON :: ObuBytes -> Value
      obuToJSON bytes = object [("data", toJSON bytes)]

decodeBitstream :: [Word8] -> Result [ObuBytes]
decodeBitstream codedBitstream =
  wrapMaybe "decode JSON" $ getBitstream <$> decode (pack codedBitstream)

encodeBitstream :: [ObuBytes] -> Result [Word8]
encodeBitstream obus = Right $ unpack $ encode $ Bitstream obus
