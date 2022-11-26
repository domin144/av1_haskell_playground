module TestTree (TestTree (TestSet, Test), process) where

data TestTree
  = TestSet String [TestTree]
  | Test String Bool

process :: TestTree -> (String, Bool)
process = processWithIndent 0
  where
    makeLine indent text = replicate (2 * indent) ' ' ++ text ++ "\n"
    makeResultLine indent label result =
      makeLine indent $ label ++ ": " ++ show result
    processWithIndent indent (TestSet label subtests) = (string, result)
      where
        processedSubTests = map (processWithIndent (indent + 1)) subtests
        result = all snd processedSubTests
        string =
          makeLine indent label
            ++ concatMap fst processedSubTests
            ++ makeResultLine indent label result
    processWithIndent indent (Test label result) = (string, result)
      where
        string = makeResultLine indent label result
