import PatternMatcher
import ExtremumMatcher
import PriceBar
import Control.Monad
import System.Environment (getArgs)

numPbs = 1000
chunkSize = 10
datasource = "../../data/USDCHF_day.csv"

main = do
    args <- getArgs
    let ok = argsOK args
    if not ok
        then usage
        else if head args == "chunk"
            then mainChunk   numPbs datasource chunkSize 
            else mainNoChunk numPbs datasource chunkSize

argsOK args = not (null args) && ((head args) `elem` ["chunk", "no-chunk"])
usage = putStrLn "SampleApp (chunk | no-chunk)"


-- n = number of price bars
-- m = chunk size
-- path = datasource
mainChunk n path m = do
    pbs <- liftM (reverse . (take n)) (pbsFromFile 4 path)
    let (ss,ems',pms') = matchChunks [] 0 0 ems pms m pbs
    putStrLn (unlines (reverse ss))
    where
        ems = initExtremumMatcherState
        pms = initPatternMatcherState pns
        pns = [bullBatInit]

matchChunks :: [String]                 -- accumulates output strings
            -> Int                      -- accumulates count of price bars
            -> Int                      -- accumulates count of extrema
            -> ExtremumMatcherState     -- starting state
            -> PatternMatcherState      -- starting state
            -> Int                      -- batch size
            -> [PriceBar]               -- price bar stream
            -> ([String], ExtremumMatcherState, PatternMatcherState)
                                        -- returns new accumulation of 
                                        --   output, new states for matchers
matchChunks output sum_pbs sum_exs ems pms _ [] = (output, ems, pms)
matchChunks output sum_pbs sum_exs ems pms batchSize pbsIn =
    let (pbs, pbTail) = splitAt batchSize pbsIn
        (exs, ems') = runExtremumMatcher (allExtrema pbs) ems
        ((),  pms') = runPatternMatcher (matchExtrema exs) pms
        sum_pbs' = sum_pbs + length pbs
        sum_exs' = sum_exs + length exs
        pns  = getPatterns pms'
        przs = getPRZs pms'
        output' = (show sum_pbs')     ++ "," ++
                  (show sum_exs')     ++ "," ++
                  (show (length pns)) ++ "," ++
                  (show (length przs))
    in matchChunks (output':output) sum_pbs' sum_exs' 
                    ems' pms' batchSize pbTail


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = (take n xs) : (chunk n (drop n xs))

mainNoChunk n path m = do
    ss <- mapM (matchPBs0 path) [m,(m+m)..n]
    putStrLn (unlines ss)

matchPBs0 :: String -> Int -> IO String
matchPBs0 path n = do
    pbs <- liftM (reverse . (take n)) (pbsFromFile 4 path)
    let (exs, ems') = runExtremumMatcher (allExtrema pbs) ems
    let ((), pms') = runPatternMatcher (matchExtrema exs) pms
    let pns = getPatterns pms'
    let przs = getPRZs pms'
    let res =   (show (length pbs)) ++ "," ++
                (show (length exs)) ++ "," ++
                (show (length pns)) ++ "," ++
                (show (length przs))
    return res

    where
        ems = initExtremumMatcherState
        pms = initPatternMatcherState pns
        pns = [bullBatInit]
