module TestFramework 
    (Test,   runTests, 
     TestIO, runTestsIO)
where

import Control.Monad

type Test = (String, Bool)
type TestIO = (String, IO Bool)
type TestResult = (String, Bool)

runTests :: [Test] -> TestResult
runTests [] = ("", True)
runTests tests =
    let results = map (\t -> runTest t) tests
        details = map (\tr -> formatDetails tr) results
        summary = all snd results
        result = (unlines details) ++ newLine
    in (result, summary)

runTestsIO :: [TestIO] -> IO TestResult
runTestsIO [] = return ("", True)
runTestsIO tests = do
    results <- mapM runTestIO tests
    let details = map (\tr -> formatDetails tr) results
    let summary = all snd results
    let result = (unlines details) ++ newLine 
    return (result, summary)

runTest :: Test -> TestResult
runTest t = (fst t, snd t)

runTestIO :: TestIO -> IO TestResult
runTestIO t = do
    result <- snd t
    return (fst t, result)

showResult :: Bool -> String
showResult b = if b then "PASS" else "FAIL"

formatDetails :: TestResult -> String
formatDetails tr = "\t" ++ (showResult (snd tr)) ++ "\t" ++ (fst tr)

newLine = "\n"
