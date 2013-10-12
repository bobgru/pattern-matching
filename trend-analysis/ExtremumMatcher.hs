module ExtremumMatcher (
    ExtremumMatcherState,
    initExtremumMatcherState,
    ExtremumMatcher,
    runExtremumMatcher,
    nextExtremum,
    allExtrema,
    getHighsLows,
    bufferList
    ) where

import Control.Monad.State
import PriceBar

data ExtremumMatcherState = ExtremumMatcherState {
      emsStrength :: Int
    , emsAllowBoth :: Bool
    , emsPriceBars :: [PriceBar]
    , emsLastHighs :: [Extremum]
    , emsLastLows :: [Extremum]
    } deriving (Show)
type ExtremumMatcher = State ExtremumMatcherState

initExtremumMatcherState :: Int -> Bool -> ExtremumMatcherState
initExtremumMatcherState str both = 
    ExtremumMatcherState {
          emsStrength  = str 
        , emsAllowBoth = both
        , emsPriceBars = []
        , emsLastHighs = []
        , emsLastLows  = []
    }

runExtremumMatcher = runState

nextExtremum :: PriceBar -> ExtremumMatcher (Maybe Extremum)
nextExtremum pb = do
    s <- get
    let str  = emsStrength  s
    let both = emsAllowBoth s
    let pbs  = emsPriceBars s
    let his  = emsLastHighs s
    let los  = emsLastLows  s

    -- Buffer new data, keeping at most strength+2 price bars.
    let pbs' = bufferPriceBars pbs (str+2) pb

    -- If there is enough data, indicate whether it's an extremum.
    let mex = nextExtremum' pbs' (str+2) both
    
    -- Update our collections of last peaks and valleys.
    let his' = bufferExtrema his Peak 2 mex   
    let los' = bufferExtrema los Valley 2 mex

    put s {
              emsPriceBars = pbs'
            , emsLastHighs = his'
            , emsLastLows  = los'
        }
    return mex

bufferList :: [a] -> Int -> a -> [a]
bufferList xs n x = if length xs < n then x:xs else x:(init xs)

bufferPriceBars :: [PriceBar] -> Int -> PriceBar -> [PriceBar]
bufferPriceBars = bufferList

bufferExtrema :: [Extremum] -> Concavity -> Int -> Maybe Extremum -> [Extremum]
bufferExtrema exs _ _ Nothing = exs
bufferExtrema exs ccv n (Just ex@(Extremum _ ccv'))
    | ccv' == oppositeConcavity ccv = exs
    | otherwise = bufferList exs 2 ex
    
oppositeConcavity Peak = Valley
oppositeConcavity Valley = Peak
oppositeConcavity _ = error "No opposite concavity to Both"

nextExtremum' :: [PriceBar] -> Int -> Bool -> Maybe Extremum
nextExtremum' []       _ _ = Nothing 
nextExtremum' [_]      _ _ = Nothing 
nextExtremum' (_:_:[]) _ _ = Nothing 
nextExtremum' pbs@(a:b:cs) str both =
    if length pbs < str 
        then Nothing
        else
            let valley = low b < lowestLow (a:cs)
                peak = high b > highestHigh (a:cs)
            in if peak && valley
                then if both then Just (Extremum b Both) else Nothing
                else if peak
                    then Just (Extremum b Peak)
                    else if valley
                        then Just (Extremum b Valley)
                        else Nothing

allExtrema :: [PriceBar] -> ExtremumMatcher [Extremum]
allExtrema pbs = 
    let purge = filter (\x -> x /= Nothing)
        unwrap = map (\(Just x) -> x)
    in do
        exs <- mapM nextExtremum pbs
        return ((unwrap . purge) exs)

getHighsLows :: ExtremumMatcher ([Extremum], [Extremum])
getHighsLows = do
    s <- get
    let his  = emsLastHighs s
    let los  = emsLastLows  s
    return (his, los)
