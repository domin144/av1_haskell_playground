import AnnexB (decodeBitstream, encodeBitstream)
import Data.Maybe (isNothing)

testDecodeBitstream :: Bool
testDecodeBitstream =
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

testEncodeBitstream :: Bool
testEncodeBitstream =
  and
    [ encodeBitstream [] == Just [],
      encodeBitstream [[[[0xff]]]]
        == Just
          [ 0x03, -- temporal_unit_size
            0x02, -- frame_unit_size
            0x01, -- obu_lenght
            0xff -- data
          ]
    ]

main = do
  putStrLn $ "testDecodeBitstream : " ++ show testDecodeBitstream
  putStrLn $ "testEncodeBitstream : " ++ show testEncodeBitstream