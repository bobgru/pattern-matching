{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ExtremumMatcher (
    ExtremumMatcherState,
    initExtremumMatcherState,
    ExtremumMatcher,
    runExtremumMatcher,
    nextExtremum,
    allExtrema
    ) where

import Control.Monad.State
import Data.Maybe (fromJust)
import PriceBar

newtype ExtremumMatcherState = EMS [PriceBar] deriving (Show)
type ExtremumMatcher = State ExtremumMatcherState

initExtremumMatcherState :: ExtremumMatcherState
initExtremumMatcherState = EMS []

runExtremumMatcher = runState

nextExtremum :: PriceBar -> ExtremumMatcher (Maybe Extremum)
nextExtremum pb = do
    -- Buffer new data, keeping at most three price bars.
    (EMS pbs) <- get
    let pbs' = case pbs of
                (a:b:_:_) -> pb:a:[b]
                otherwise  -> pb:pbs
    put (EMS pbs')
    
    -- If there is enough data, indicate whether it's an extremum.
    case pbs' of
        (a:b:c:_) -> return (getExtremum a b c)
        otherwise  -> return Nothing

allExtrema :: [PriceBar] -> ExtremumMatcher [Extremum]
allExtrema pbs = 
    let purge = filter (/= Nothing)
        unwrap = map fromJust
    in do
        exs <- mapM nextExtremum pbs
        return ((unwrap . purge) exs)
