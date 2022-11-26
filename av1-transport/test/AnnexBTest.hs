module AnnexBTest (annexBTest) where

import AnnexB (decodeStructuredBitstream, encodeStructuredBitstream)
import Data.Maybe (isNothing)

decodeBitstream = decodeStructuredBitstream

encodeBitstream = encodeStructuredBitstream

decodeBitstreamTest :: Bool
decodeBitstreamTest =
  and
    [ decodeBitstream [] == Just [],
      decodeBitstream [0x00] == Just [[]],
      isNothing $ decodeBitstream [0x01],
      decodeBitstream [0x01, 0x00] == Just [[[]]],
      isNothing $ decodeBitstream [0x01, 0x00, 0xff],
      decodeBitstream
        [ 0x03, -- temporal_unit_size
          0x02, -- frame_unit_size
          0x01, -- obu_lenght
          0xff -- data
        ]
        == Just [[[[0xff]]]],
      decodeBitstream
        [ 0x0e, -- temporal_unit_size
          0x06, -- frame_unit_size
          0x02, -- obu_lenght
          0xff,
          0xff,
          0x02, -- obu_lenght
          0xff,
          0xff,
          0x06, -- frame_unit_size
          0x02, -- obu_lenght
          0xff,
          0xff,
          0x02, -- obu_lenght
          0xff,
          0xff
        ]
        == Just [replicate 2 (replicate 2 (replicate 2 0xff))]
    ]

encodeBitstreamTest :: Bool
encodeBitstreamTest =
  and
    [ encodeBitstream [] == Just [],
      encodeBitstream [[[[0xff]]]]
        == Just
          [ 0x03, -- temporal_unit_size
            0x02, -- frame_unit_size
            0x01, -- obu_lenght
            0xff -- data
          ],
      encodeBitstream [replicate 2 (replicate 2 (replicate 2 0xff))]
        == Just
          [ 0x0e, -- temporal_unit_size
            0x06, -- frame_unit_size
            0x02, -- obu_lenght
            0xff,
            0xff,
            0x02, -- obu_lenght
            0xff,
            0xff,
            0x06, -- frame_unit_size
            0x02, -- obu_lenght
            0xff,
            0xff,
            0x02, -- obu_lenght
            0xff,
            0xff
          ]
    ]

annexBTest = do
  putStrLn "AnnexBTest"
  putStrLn $ "decodeBitstreamTest : " ++ show decodeBitstreamTest
  putStrLn $ "encodeBitstreamTest : " ++ show encodeBitstreamTest
