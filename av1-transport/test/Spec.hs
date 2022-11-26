import AnnexBTest (annexBTest)
import CmdArgsTest (cmdArgsTest)
import CommonTest (commonTest)
import JsonTest (jsonTest)
import LowOverheadTest (lowOverheadTest)
import ObuHeaderTest (obuHeaderTest)
import System.Exit
import TestTree (TestTree (TestSet), process)

tests :: TestTree
tests =
  TestTree.TestSet
    "All tests"
    [ commonTest,
      cmdArgsTest,
      obuHeaderTest,
      lowOverheadTest,
      annexBTest,
      jsonTest
    ]

main :: IO ()
main = do
  let (text, result) = process tests
  putStr text
  if result
    then System.Exit.exitSuccess
    else System.Exit.exitFailure
