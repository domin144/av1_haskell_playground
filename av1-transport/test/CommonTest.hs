module CommonTest (commonTest) where

import Common (at, decodeLeb128, encodeLeb128, maybeSplit)
import Data.Maybe (isNothing)

testDecodeLeb128 :: Bool
testDecodeLeb128 =
  and
    [ isNothing (decodeLeb128 (replicate 8 0x80)),
      decodeLeb128 (replicate 7 0x80 ++ [0, 1, 2]) == Just (0, 8, [1, 2]),
      decodeLeb128 [0x7f, 1, 2] == Just (0x7f, 1, [1, 2]),
      decodeLeb128 [0xf5, 0x3, 1, 2] == Just (0x1f5, 2, [1, 2]),
      decodeLeb128 (replicate 7 0xff ++ [0x7f, 2, 3]) == Just (0xffffffffffffff, 8, [2, 3])
    ]

testEncodeLeb128 :: Bool
testEncodeLeb128 =
  and
    [ encodeLeb128 0 == Just ([0x00], 1),
      encodeLeb128 0x7f == Just ([0x7f], 1),
      encodeLeb128 0x1f5 == Just ([0xf5, 0x03], 2),
      encodeLeb128 0xffffffffffffff == Just (replicate 7 0xff ++ [0x7f], 8),
      isNothing (encodeLeb128 0x100000000000000)
    ]

testEncodeDecodeLeb128 :: Bool
testEncodeDecodeLeb128 =
  all
    test
    [ 0,
      1,
      0x7f,
      0xff,
      0x1ff,
      0x10,
      0x20,
      0x40,
      0x80,
      0x100,
      0x200,
      0x400,
      0x800,
      0x3332,
      0xffff,
      0x123f3f3f3,
      0x123f3f3f3,
      0x123f3f3f3,
      0x5a5a5a5a5a5,
      0xffffffffffffff
    ]
  where
    test x =
      do
        (encodedBytes, encodedSize) <- encodeLeb128 x
        (decodedX, decodedSize, remainder) <- decodeLeb128 encodedBytes
        return (x == decodedX && null remainder)
        == Just True

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

commonTest = do
  putStrLn "CommonTest"
  putStrLn $ "testDecodeLeb128 : " ++ show testDecodeLeb128
  putStrLn $ "testEncodeLeb128 : " ++ show testEncodeLeb128
  putStrLn $ "testEncodeDecodeLeb128 : " ++ show testEncodeDecodeLeb128
  putStrLn $ "testMaybeSplit : " ++ show testMaybeSplit
  putStrLn $ "testAt : " ++ show testAt
