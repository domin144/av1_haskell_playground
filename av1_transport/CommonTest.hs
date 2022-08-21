import Common (at, decodeLeb128, maybeSplit)
import Data.Maybe (isNothing)

testDecodeLeb128 =
  and
    [ isNothing (decodeLeb128 (replicate 8 0x80)),
      decodeLeb128 (replicate 7 0x80 ++ [0, 1, 2]) == Just (0, 8, [1, 2]),
      decodeLeb128 [0x7f, 1, 2] == Just (0x7f, 1, [1, 2]),
      decodeLeb128 [0xf5, 0x3, 1, 2] == Just (0x1f5, 2, [1, 2])
    ]

testMaybeSplit :: Bool
testMaybeSplit =
  and
    [ isNothing (maybeSplit 1 []),
      maybeSplit 0 ([] :: [Integer]) == Just ([], []),
      maybeSplit 0 [1] == Just ([], [1]),
      maybeSplit 1 [1] == Just ([1], []),
      maybeSplit 3 [10, 11, 12, 13, 14] == Just ([10, 11, 12], [13, 14])
    ]

testAt :: Bool
testAt =
  and
    [ isNothing (at [] 0),
      at [1] 0 == Just 1,
      isNothing (at [1] 1),
      at [10, 11, 12] 1 == Just 11
    ]

main = do
  putStrLn $ "testDecodeLeb128 : " ++ show testDecodeLeb128
  putStrLn $ "testMaybeSplit : " ++ show testMaybeSplit
  putStrLn $ "testAt : " ++ show testAt
