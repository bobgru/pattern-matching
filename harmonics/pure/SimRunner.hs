import System.Environment
import Harmonics
import PriceBar


myExtremaStrength = 3
myRwRk = 1.5 :: Price
myRisk = 21.0 :: Price
myRwPct = 0.50 :: Price
myTicksAfterD = 3
myMinPipsDiff = 100 :: Price


main = do
    [t] <- getArgs
    case t of
        "test1" -> test1
        "test2" -> test2
        "test3" -> test3
        "test4" -> test4
        "test5" -> test5

testExtrema = do
    --pbs <- pbsFromFile 4 "../../../data/USDCHF_day.csv"
    pbs <- pbsFromFile 4 "../../data/USDCHF_hour.csv"
    let exts = take 200 (extrema myExtremaStrength pbs)
    return (pbs, exts)

test f = do
    (pbs, exts) <- testExtrema
    let pn = gartley
    f pn myMinPipsDiff pbs exts

test' f = do
    (pbs, exts) <- testExtrema
    let pns = [gartley, bat, butterfly, crab]
    f pns myMinPipsDiff pbs exts

test1 = test $ \p d pbs xs -> do
                        let n = length (seqs d (numPbs p) xs)
                        putStrLn ("length seqs = " ++ show n ++ "\n")

test2 = test $ \p d pbs xs -> do
                        let ms = allMatches [p] d xs
                        putStrLn (formatMatches ms)

test3 = test $ \p d pbs xs -> do
                        let ms = allMatches [p] d xs
                        let os = outcomes ms pbs xs
                        putStrLn (formatMatchesWithResults os)

test4 = test $ \p d pbs xs -> do
                        let ms = allMatches [p] d xs
                        --putStrLn (show (length ms))
                        let os = outcomes (coalesce ms) pbs xs
                        --putStrLn (show (length os))
                        putStrLn (formatMatchesWithResults os)

test5 = test' $ \pns d pbs xs -> do
                        let ms = allMatches pns d xs
                        putStrLn ("Matches=" ++ show (length ms))
                        let ms' = coalesce ms
                        putStrLn ("Filtered for same BD=" ++ show (length ms'))
                        let ms'' = filter (acceptRwRk myRisk myRwPct myRwRk)  ms'
                        putStrLn ("Filtered for reward:risk=" ++ show (length ms''))
                        let os = outcomes ms'' pbs xs
                        putStrLn ("Results=" ++ show (length os))
                        putStrLn (formatMatchesWithResults os)


outcomes :: [Match Price]
         -> [PriceBar Price]
         -> [PriceBar Price]
         -> [(Match Price, String)]
outcomes ms pbs xs = [(m, r) | m <- ms
                             , let r = outcome (snd m) pbs xs]

outcome :: [PriceBar Price]
        -> [PriceBar Price]
        -> [PriceBar Price]
        -> String
outcome m pbs xs = result
    where
        result =
            if rwrk < myRwRk
                then fmtNoTrade rwrk
                else case (rkpbs, rwpbs) of
                            ([],   []) -> fmtIndeterminate
                            ([],  y:_) -> fmtWinner rw y rwrk
                            (x:_,  []) -> fmtLoser  rk x rwrk
                            (x:_, y:_) -> if tick x > tick y
                                            then fmtWinner rw y rwrk
                                            else fmtLoser  rk x rwrk
        rkpbs = filter (exceeds rkpr rkLong) pbs'
        rwpbs = filter (exceeds rwpr rwLong) pbs'
        pbs'  = dropWhile ((td >=) . tick) pbs
        (rk, rkpr, rkLong) = risk   myRisk  m
        (rw, rwpr, rwLong) = reward myRwPct m
        rwrk = rw / rk
        td = tick (m !! 4) + myTicksAfterD
        b = m !! 2

fmtRewardRisk :: (Fractional a, Show a) => a -> String
fmtRewardRisk rwrk = "Reward:Risk = " ++ show rwrk

fmtNoTrade :: (Fractional a, Show a) => a -> String
fmtNoTrade rwrk = "Trade not taken, " ++ fmtRewardRisk rwrk

fmtResult :: (Ord a, Fractional a, Show a) => 
             String -> a -> PriceBar a -> a -> String
fmtResult msg amt pb rwrk = msg ++ " " ++ show amt
                                ++ " at tick " ++ show (tick pb)
                                ++ ", " ++ fmtRewardRisk rwrk

fmtWinner :: (Ord a, Fractional a, Show a) => a -> PriceBar a -> a -> String
fmtWinner = fmtResult "Gain"

fmtLoser :: (Ord a, Fractional a, Show a) => a -> PriceBar a -> a -> String
fmtLoser  = fmtResult "Loss"

fmtIndeterminate :: String
fmtIndeterminate  = "Indeterminate"


contains pr pb = low pb <= pr  &&  pr <= high pb

exceeds :: (Ord a, Num a) => a -> Bool -> PriceBar a -> Bool
exceeds pr long pb =
    if long
        then high pb >= pr
        else low  pb <= pr

formatMatches :: (Fractional a, Show a) => [Match a] -> String
formatMatches = unlines . map formatMatch

formatMatch :: (Fractional a, Show a) => Match a -> String
formatMatch (nm, xs) = nm ++ ":\n" 
                          ++ unlines [ fmt (tick x, peak x, 
                                if peak x then high x else low x) |
                                x <- xs ]
    where
        fmt :: Show a => (Int, Bool, a) -> String 
        fmt (t,p,v) = show t ++ " " ++ show p ++ " " 
                            ++ (if p then " " else "") ++ show v

formatMatchesWithResults :: (Fractional a, Show a) => 
                            [(Match a, String)] -> String
formatMatchesWithResults = unlines . map formatMatchWithResult

formatMatchWithResult :: (Fractional a, Show a) => (Match a, String) -> String
formatMatchWithResult ((nm, xs), r) = nm ++ ":" ++ r ++ "\n" 
                           ++ unlines [ fmt (tick x, peak x, 
                                if peak x then high x else low x) |
                                x <- xs ]
    where
        fmt :: Show a => (Int, Bool, a) -> String 
        fmt (t,p,v) = show t ++ " " ++ show p ++ " " 
                            ++ (if p then " " else "") ++ show v


