module Main where
import ExtremumMatcher
import PriceBar
import TestFramework
import Control.Monad
import System.Exit (exitFailure, exitSuccess)

main = do
    let (details, summary) = runTests pureTests
    putStrLn details
    (ioDetails, ioSummary) <- runTestsIO ioTests
    putStrLn ioDetails

    if not (summary && ioSummary) then exitFailure else exitSuccess


pureTests = [
      ("firstPB", firstPB)
    , ("secondPB", secondPB)
    , ("thirdPB_Nothing", thirdPB_Nothing)
    , ("thirdPB_Peak", thirdPB_Peak)
    , ("thirdPB_Valley", thirdPB_Valley)
    , ("pbsToExtrema", pbsToExtrema)
    , ("pbsToExtrema_TwoParts", pbsToExtrema_TwoParts)
    ]

ioTests = [
      ("csvToExtrema", csvToExtrema)
    ]

newPB :: Price -> PriceBar
newPB p = Pb "2020-01-01" 0 p p p p

a = newPB 10
b = newPB 20
c = newPB 15
d = newPB 30

(r,s) = runExtremumMatcher (nextExtremum a) initExtremumMatcherState

addPB :: PriceBar -> ExtremumMatcherState -> (Maybe Extremum, ExtremumMatcherState)
addPB pb s = runExtremumMatcher (nextExtremum pb) s

firstPB = pass
    where
        (r,s) = addPB a initExtremumMatcherState
        pass = (r == Nothing)

secondPB = pass
    where
        (r,s)   = addPB a initExtremumMatcherState
        (r',s') = addPB b s
        pass = (r' == Nothing)

thirdPB_Nothing = pass
    where
        (r,s)     = addPB a initExtremumMatcherState
        (r',s')   = addPB b s
        (r'',s'') = addPB d s'
        pass = (r'' == Nothing)
    
thirdPB_Peak = pass
    where
        (r,s)     = addPB a initExtremumMatcherState
        (r',s')   = addPB b s
        (r'',s'') = addPB c s'
        pass = (r'' == Just (Ext b Peak))

thirdPB_Valley = pass
    where
        (r,s)     = addPB b initExtremumMatcherState
        (r',s')   = addPB a s
        (r'',s'') = addPB c s'
        pass = (r'' == Just (Ext a Valley))

pbsToExtrema = pass
    where
        ps = concat [[1..10], [9..5], [6..15], [14..10], [11..20]]
        pbs = [newPB p | p <- ps]
        s = initExtremumMatcherState
        (exs, s') = runExtremumMatcher (allExtrema pbs) s
        pass = length exs == 4

pbsToExtrema_TwoParts = pass
    where
        ps1 = concat [[1..10], [9..5], [6..15]]
        ps2 = concat [[14..10], [11..20]]
        pbs1 = [newPB p | p <- ps1]
        pbs2 = [newPB p | p <- ps2]
        s = initExtremumMatcherState
        (exs1, s1)  = runExtremumMatcher (allExtrema pbs1) s
        (exs2, s2) = runExtremumMatcher (allExtrema pbs2) s1
        pass = (length exs1 == 2) && (length exs2 == 2)

-- Standalone test (because it requires IO monad).
-- Returns:
-- Length pbs = 2683
-- Length exs = 1155
-- True

csvToExtrema :: IO Bool
csvToExtrema = do
    pbs <- pbsFromFile 4 path
    let (exs, s') = runExtremumMatcher (allExtrema pbs) s
    --putStrLn ("Length pbs = " ++ (show (length pbs)))
    --putStrLn ("Length exs = " ++ (show (length exs)))
    return (length exs == 1155)
    where
        path = "../../data/USDCHF_day.csv"
        s = initExtremumMatcherState

countExtremaInPbs :: Int -> IO Int
countExtremaInPbs n = do
    pbs <- liftM (take n) (pbsFromFile 4 path)
    let (exs, s') = runExtremumMatcher (allExtrema pbs) s
    return (length exs)
    where
        path = "../../data/USDCHF_day.csv"
        s = initExtremumMatcherState
