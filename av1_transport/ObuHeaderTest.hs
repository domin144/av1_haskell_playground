import ObuHeader

bytesToBitsTest :: Bool
bytesToBitsTest =
  bytesToBits [0xf5, 0x3]
    == [ True, -- 0
         True,
         True,
         True,
         False, -- 4
         True,
         False,
         True,
         False, -- 8
         False,
         False,
         False,
         False, -- 12
         False,
         True,
         True
       ]

decodeFixedTest =
  and
    [ decodeFixed 1 [] == Nothing,
      decodeFixed 3 [True, True, False, False] == Just (6, [False])
    ]

main = do
  putStrLn $ "bytesToBitsTest : " ++ show bytesToBitsTest
  putStrLn $ "decodeFixedTest : " ++ show decodeFixedTest