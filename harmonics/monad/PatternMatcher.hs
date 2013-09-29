module PatternMatcher (
    Pattern(..),
    PRZ(..),
    PatternMatchResult,
    PatternMatcherState(..),
    initPatternMatcherState,
    PatternMatcher,
    runPatternMatcher,
    matchExtrema,
    matchPatterns,
    matchPattern,
    bullBatInit
    ) where

import Control.Monad.State
import PriceBar
import ExtremumMatcher

type PatternMatchResult = (Maybe Pattern, Maybe PRZ)
data Pattern = Pattern {
    points :: [Extremum],       -- e.g. D, C, B, A, X
    targetRange :: PriceRange,
    evalMatch :: Pattern -> Extremum -> PatternMatchResult
    }

data PRZ = PRZ { 
    prz_points :: [Extremum],
    prz_range  :: PriceRange
    }

data PatternMatcherState = PatternMatcherState {
    getPatterns :: [Pattern],
    getPRZs     :: [PRZ]
}
type PatternMatcher = State PatternMatcherState

initPatternMatcherState :: [Pattern] -> PatternMatcherState
initPatternMatcherState pns = PatternMatcherState pns []

runPatternMatcher = runState

matchExtrema :: [Extremum] -> PatternMatcher ()
matchExtrema [] = return ()
matchExtrema (x:xs) = do
    (PatternMatcherState pns przs) <- get
    let (pns', przs') = matchPatterns pns x
    put (PatternMatcherState (pns++pns') (przs++przs'))
    matchExtrema xs

matchPatterns :: [Pattern] -> Extremum -> ([Pattern], [PRZ])
matchPatterns [] _ = ([],[])
matchPatterns pns x = (pns', przs')
    where
        something Nothing = False
        something _       = True
        purge  = filter something
        unwrap = map (\(Just x) -> x)
        
        res    = map (matchPattern x) pns
        pns'   = unwrap (purge (map fst res))
        przs'  = unwrap (purge (map snd res))

matchPattern :: Extremum -> Pattern -> PatternMatchResult
matchPattern x p = evalMatch p p x

bullBatInit :: Pattern
bullBatInit = Pattern [] (PriceRange 0 0) bullBatMatchX

bullBatMatchX :: Pattern -> Extremum -> PatternMatchResult
bullBatMatchX _ (Ext _ Peak) = (Nothing, Nothing)
bullBatMatchX _ x = (Just (Pattern [x] (PriceRange 0 0) bullBatMatchA), Nothing)
--bullBatMatchX _ x = (Nothing, Just (PRZ [x] (PriceRange 0 0)))

bullBatRangeB :: [Extremum] -> PriceRange
bullBatRangeB ((Ext a Peak):(Ext x Valley):[]) = 
    PriceRange newRangeLo newRangeHi
    where
        xa = high a - low x
        newRangeLo = (high a - xa * 0.5)
        newRangeHi = (high a - xa * 0.382)
bullBatRangeB _ = error "bullBatRangeB given wrong points"

bullBatMatchA :: Pattern -> Extremum -> PatternMatchResult
bullBatMatchA _ (Ext _ Valley) = (Nothing, Nothing)
bullBatMatchA m a = (pn', Nothing)
    where
        (Ext x _) = head (points m)
        (Ext a' _) = a
        xa = high a' - low x
        points' = a : (points m)
        range' = bullBatRangeB points'
        evalMatch' = bullBatMatchB
        pn' = if xa > 0
            then Just (Pattern points' range' evalMatch')
            else Nothing
{-
bullBatMatchA m a = 
    (Just (Pattern points' range' evalMatch'), Nothing)
--    (Nothing, Just (PRZ points' range'))
    where
        points' = a : (points m)
        range' = bullBatRangeB points'
        evalMatch' = bullBatMatchB
-}


bullBatRangeC :: [Extremum] -> PriceRange
bullBatRangeC ((Ext b Valley):(Ext a Peak):(Ext x Valley):[]) = 
    PriceRange newRangeLo newRangeHi
    where
        xa = high a - low x
        ab = high a - low b
        newRangeLo = low b + ab * (max 0.382 ((0.886*xa/ab - 1)/1.618))
        newRangeHi = low b + ab * (min 0.886 ((0.886*xa/ab - 1)/0.618))
bullBatRangeC _ = error "bullBatRangeC given wrong points"

bullBatMatchB :: Pattern -> Extremum -> PatternMatchResult
bullBatMatchB _ (Ext _ Peak) = (Nothing, Nothing)
bullBatMatchB m b =
    if not (isValleyInRange (targetRange m) b)
        then (Nothing, Nothing)
        else
            let points' = b : (points m)
                range' = bullBatRangeC points'
                evalMatch' = bullBatMatchC
            in (Just (Pattern points' range' evalMatch'), Nothing)
--            in (Nothing, Just (PRZ points' range'))

bullBatRangeD :: [Extremum] -> PriceRange
bullBatRangeD ((Ext c Peak):(Ext b Valley):(Ext a Peak):(Ext x Valley):[]) = 
    PriceRange newRangeLo newRangeHi
    where
        xa = high a - low x
        ab = high a - low b
        bc = high c - low b
        newRangeLo = high c - bc * 2.618
        newRangeHi = high c - bc * 1.618
bullBatRangeD _ = error "bullBatRangeD given wrong points"

bullBatMatchC :: Pattern -> Extremum -> PatternMatchResult
bullBatMatchC _ (Ext _ Valley) = (Nothing, Nothing)
bullBatMatchC m c =
    if not (isPeakInRange (targetRange m) c)
        then (Nothing, Nothing)
        else
            let points' = c : (points m)
                range' = bullBatRangeD points'
                evalMatch' = bullBatMatchD
                in (Just (Pattern points' range' evalMatch'), Nothing)

bullBatMatchD :: Pattern -> Extremum -> PatternMatchResult
bullBatMatchD _ (Ext _ Peak) = (Nothing, Nothing)
bullBatMatchD m d@(Ext d_pb _) =
    let d_lo = low d_pb
        (Ext b_pb _) = (points m) !! 1
        b_lo = low b_pb
        pips = 0.013        --TODO magic
        reward_risk = 2     --TODO magic
        (PriceRange _ prz_lo) = targetRange m
        risk_reward_ok = d_lo <= (b_lo + reward_risk * (prz_lo + pips))
    in if not ((isValleyInRange (targetRange m) d) && risk_reward_ok)
        then (Nothing, Nothing)
        else
            let points' = d : (points m)
                range' = targetRange m
            in (Nothing, Just (PRZ points' range'))


{-
calcRewardRisk prz d = (b' - d') / (d - bottom - buffer)
    where
        b = (prz_points prz) !! 2
        
        b' = case b of
            Extremum pb_b Peak
-}            