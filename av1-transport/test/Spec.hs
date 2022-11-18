import AnnexBTest (annexBTest)
import CmdArgsTest (cmdArgsTest)
import CommonTest (commonTest)
import LowOverheadTest (lowOverheadTest)
import ObuHeaderTest (obuHeaderTest)

main :: IO ()
main = do
  commonTest
  cmdArgsTest
  obuHeaderTest
  lowOverheadTest
  annexBTest