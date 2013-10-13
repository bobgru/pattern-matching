module TestFramework 
    (Test,   runTests, 
     TestIO, runTestsIO,
     assertAreEqual)
where

import Control.Monad

type Test = (String, (Bool, String))
type TestIO = (String, IO (Bool, String))
type TestResult = (String, Bool, String)

runTests :: [Test] -> (String, Bool)
runTests [] = ("", True)
runTests tests =
    let results = map runTest tests
        details = map formatDetails results
        summary = all snd3 results
        result = unlines details ++ newLine ++ showResult summary
    in (result, summary)

runTestsIO :: [TestIO] -> IO (String, Bool)
runTestsIO [] = return ("", True)
runTestsIO tests = do
    results <- mapM runTestIO tests
    let details = map formatDetails results
    let summary = all snd3 results
    let result = unlines details ++ newLine ++ showResult summary 
    return (result, summary)

runTest :: Test -> TestResult
runTest t = (fst t, (fst . snd) t, (snd . snd) t)

runTestIO :: TestIO -> IO TestResult
runTestIO t = do
    result <- snd t
    return (fst t, fst result, snd result)

showResult :: Bool -> String
showResult b = if b then "PASS" else "FAIL"

formatDetails :: TestResult -> String
formatDetails tr = showResult (snd3 tr) ++ "  " ++ fst3 tr
                    ++ errmsg
    where errmsg = if snd3 tr then "" else newLine ++ "  " ++ thd3 tr

newLine = "\n"

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

assertAreEqual :: (Eq a, Show a) => a -> a -> (Bool, String)
assertAreEqual expected actual = (b, s)
    where
        b = expected == actual
        s = "Expected=" ++ show expected ++ " Actual=" ++ show actual

