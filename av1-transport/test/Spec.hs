import AnnexBTest (annexBTest)
import CmdArgsTest (cmdArgsTest)
import CommonTest (commonTest)
import JsonTest (jsonTest)
import LowOverheadTest (lowOverheadTest)
import ObuHeaderTest (obuHeaderTest)
import TestTree (TestTree (SubTree, Test), process)

someTest :: TestTree
someTest =
  SubTree "SomeTest"
    [ Test "Test" True,
      Test "Test2" False
    ]

main :: IO ()
main = do
  commonTest
  cmdArgsTest
  obuHeaderTest
  lowOverheadTest
  annexBTest
  jsonTest
  putStr $ fst $ process someTest
