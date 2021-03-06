import Common (at, decodeLeb128, maybeSplit)
import Data.Maybe (isNothing)

testLeb128_0 :: Bool
testLeb128_0 =
  isNothing (decodeLeb128 (replicate 8 0x80))

testLeb128_1 :: Bool
testLeb128_1 =
  decodeLeb128 (replicate 7 0x80 ++ [0, 1, 2]) == Just (0, 8, [1, 2])

testLeb128_2 :: Bool
testLeb128_2 =
  decodeLeb128 [0x7f, 1, 2] == Just (0x7f, 1, [1, 2])

testLeb128_3 :: Bool
testLeb128_3 =
  decodeLeb128 [0xf5, 0x3, 1, 2] == Just (0x1f5, 2, [1, 2])

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
  putStrLn $ "testLeb128_0 : " ++ show testLeb128_0
  putStrLn $ "testLeb128_1 : " ++ show testLeb128_1
  putStrLn $ "testLeb128_2 : " ++ show testLeb128_2
  putStrLn $ "testLeb128_3 : " ++ show testLeb128_3
  putStrLn $ "testMaybeSplit : " ++ show testMaybeSplit
  putStrLn $ "testAt : " ++ show testAt
