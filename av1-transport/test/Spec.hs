import AnnexBTest (annexBTest)
import CmdArgsTest (cmdArgsTest)
import CommonTest (commonTest)
import LowOverheadTest (lowOverheadTest)
import ObuHeaderTest (obuHeaderTest)
import JsonTest (jsonTest)

main :: IO ()
main = do
  commonTest
  cmdArgsTest
  obuHeaderTest
  lowOverheadTest
  annexBTest
  jsonTest