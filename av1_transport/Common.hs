module Common (TransportFormat (..), Parameters (..)) where

data TransportFormat = LowOverhead | AnnexB
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Parameters = Parameters
  { inputFormat :: TransportFormat,
    inputFileName :: String,
    outputFormat :: TransportFormat,
    outputFileName :: String
  }
  deriving (Eq, Show, Read)