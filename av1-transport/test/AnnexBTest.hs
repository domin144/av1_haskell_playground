module AnnexBTest (annexBTest) where

import AnnexB (decodeStructuredBitstream, encodeStructuredBitstream)
import Data.Maybe (isNothing)
import TestTree (TestTree (Test, TestSet))

decodeBitstream = decodeStructuredBitstream

encodeBitstream = encodeStructuredBitstream

decodeBitstreamTest :: TestTree
decodeBitstreamTest =
  TestSet
    "decodeBitstreamTest"
    [ Test "null" $ decodeBitstream [] == Just [],
      Test "0x00" $ decodeBitstream [0x00] == Just [[]],
      Test "0x01" $ isNothing $ decodeBitstream [0x01],
      Test "0x01, 0x00" $ decodeBitstream [0x01, 0x00] == Just [[[]]],
      Test "underflow" $ isNothing $ decodeBitstream [0x01, 0x00, 0xff],
      Test "single obu" $
        decodeBitstream
          [ 0x03, -- temporal_unit_size
            0x02, -- frame_unit_size
            0x01, -- obu_lenght
            0xff -- data
          ]
          == Just [[[[0xff]]]],
      Test "2 x 2 x 2" $
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

encodeBitstreamTest :: TestTree
encodeBitstreamTest =
  TestSet
    "encodeBitstreamTest"
    [ Test "null" $ encodeBitstream [] == Just [],
      Test "single obu" $
        encodeBitstream [[[[0xff]]]]
          == Just
            [ 0x03, -- temporal_unit_size
              0x02, -- frame_unit_size
              0x01, -- obu_lenght
              0xff -- data
            ],
      Test "2 x 2 x 2" $
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

annexBTest :: TestTree
annexBTest =
  TestSet
    "annexBTest"
    [ decodeBitstreamTest,
      encodeBitstreamTest
    ]
