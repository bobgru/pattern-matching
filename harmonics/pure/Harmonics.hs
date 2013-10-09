module Harmonics where

import Data.List(tails,subsequences,sort,sortBy,nubBy)
import PriceBar

-- |Produce all tails of a list of exactly the specified length.
tailsOfLength :: Int -> [a] -> [[a]]
tailsOfLength 0  _ = []
tailsOfLength _ [] = []
tailsOfLength n xs = f xs
    where f = filter ((==n) . length) . map (take n) . tails

-- |A property of a list of pricebars.
type PBProp a = [PriceBar a] -> Bool

-- |A description of a pattern of a list of pricebars.
data Pattern a = Pn {
      name   :: String          -- ^The pattern name
    , numPbs :: Int             -- ^The number of pricebars in the pattern
    , hard   :: [PBProp a]      -- ^Hard constraints for match
    , soft   :: [PBProp a]      -- ^Soft constraints for match
    }

type Match a = (String, [PriceBar a])

-- |Calculate the larger difference between extremes of two pricebars.
-- This function is intended to calculate the swing between
-- successive peaks and valleys or vice versa.
pipsDiff :: (Ord a, Num a) => PriceBar a -> PriceBar a -> a
pipsDiff a b
    | peak a == pb  = error "pipsDiff applied to same-type extrema"
    | otherwise     = if pb then up else down
    where
        pb   = peak b
        up   = high b - low a
        down = low b - high a

-- |Calculate the time difference between two pricebars.
-- TODO approximated with Int
timeDiff :: (Ord a, Num a) => PriceBar a -> PriceBar a -> Int
timeDiff a b
    | tick a < tick b  =  tick b - tick a
    | otherwise        =  timeDiff b a

-- |Test whether a number is within a desired distance of a goal.
-- It is an error for the goal to be less than the tolerance.
equalWithin :: (Ord a, Fractional a, Show a) => a -> a -> a -> Bool
equalWithin eps testVal goal
    | goal > eps   =  abs (testVal - goal) / goal < eps
    | otherwise =  error msg
    where msg = "equalWithin: goal (" ++ show goal ++ ")" 
                ++ " is less than tolerance (" ++ show eps ++ ")"

-- |Decide whether a number is close to one of a list of numbers,
-- within a given tolerance.
closeToOneOf :: (Ord a, Fractional a, Show a)
             =>  a          -- ^The ratio to test
             -> [a]         -- ^The target ratios
             ->  a          -- ^The allowed deviation from the target
             -> Bool        -- ^True if VW / UV is close enough to the target
closeToOneOf testVal goals eps =  any (equalWithin eps testVal) goals

extrema :: Ord a
        => Int                  -- ^The strength of the extremum, i.e. the
                                -- number of preceding extrema of the same
                                -- type but of lesser significance 
        -> [PriceBar a]         -- ^The raw pricebars
        -> [PriceBar a]         -- ^The extrema
extrema s pbs = 
    [ pb { ext = True, peak = peak' } | 
          pbs' <- tailsOfLength (s + 2) pbs
        , let pb      = last (init pbs')
        , let peak'   = high pb == highestHigh pbs'
        , let valley' = low  pb == lowestLow   pbs'
        , (peak' || valley') && not (peak' && valley') ]

-- |Assumes all pricebars are extrema.
oppositePeaks :: Ord a => [PriceBar a] -> Bool
oppositePeaks pbs = all test pbs'
    where
        pbs' = zip pbs (tail pbs)
        test (a,b) = pa /= pb  &&  
                        ((pa  &&  high a > low b) ||
                         (pb  &&  high b > low a))
            where pa = peak a; pb = peak b

extOK :: (Num a, Ord a) => a -> PriceBar a -> PriceBar a -> Bool
extOK d x y  =  (peak x /= peak y) &&
                    ((peak x  &&  pipsDiff x y <= (-d)) ||
                     (peak y  &&  pipsDiff x y >=   d))

-- |Produce n-bar subsequences of alternating extrema in which
-- each swing is at least  d  pips, starting from the first bar.
-- The input stream must only include extrema.
seqs :: (Ord a, Num a) => a -> Int -> [PriceBar a] -> [[PriceBar a]]
seqs _ _    []   =  []
seqs _ 0     _   =  []
seqs _ 1    xs   =  [[x] | x <- xs]
seqs d n (x:xs)  =  seqs' d (n-1) x xs  ++  seqs d n xs

-- |Produce n-bar subsequences of alternating extrema in which
-- each swing is at least  d  pips, starting from the first bar.
-- The input stream must only include extrema.
seqs' :: (Ord a, Num a) => a -> Int -> PriceBar a -> [PriceBar a] -> [[PriceBar a]]
seqs' _ _ _ []  = []
seqs' _ 0 _  _  = []
seqs' d 1 x xs  = [[x,y] | y  <- filter (extOK d x) xs]
seqs' d n x xs  = [x:xs' | xs'<- (concatMap (tailSeqs d (n-1))
                                         . goodTails d x) xs]

tailSeqs d n (x,xs) = seqs' d n x xs

goodTails _ _    []  = []
goodTails d x (y:ys) = if extOK d x y
                    then (y,ys) : goodTails d x ys
                    else          goodTails d x ys

-- |Decide whether the prefix of a stream of pricebars matches
-- a pattern, considering only the hard constraints.
match :: Pattern a -> [PriceBar a] -> Bool
match pn pbs
    | null (hard pn) = True
    | null pbs       = False
    | otherwise      = allOf (hard pn)
    where
        allOf = all (\f -> f pbs)

-- |Decide whether the prefix of a stream of pricebars matches a pattern,
-- where at least one of the soft constraints must hold.
matchSoft :: Pattern a -> [PriceBar a] -> Bool
matchSoft pn pbs     =  match pn pbs  &&  anyOf (soft pn)
    where anyOf xs   =  not (null xs)  &&  (or  . match') xs
          match'     =  map (\f -> f pbs)

-- |Decide whether the prefix of a stream of pricebars matches a pattern,
-- where n of the soft constraints must hold.
matchCount :: Pattern a -> Int -> [PriceBar a] -> Bool
matchCount pn n pbs  =  match pn pbs  &&  n <= countOf (soft pn)
    where countOf    =  length . filter id . match'
          match'     =  map (\f -> f pbs)

allMatches :: (Ord a, Fractional a)
           => [Pattern a] 
           ->  a
           -> [PriceBar a] 
           -> [Match a]
allMatches pns d
    | minimum ns /= maximum ns
        = error "allMatches applied to patterns of different sizes"
    | otherwise = concatMap (ms . matchPns pns) . seqs d n
    where
        ns   = map numPbs pns
        n    = head ns
        ms   = filter (not . null . snd)
        matchPns pns pbn = map (matchPn pbn) pns
        matchPn  pbn pn  = if match pn pbn then (name pn, pbn) else ("", [])

-- |Eliminate patterns with identical B and D points.
-- For remaining patterns with identical D points, take
-- the one with the largest B-D distance.
coalesce :: (Ord a, Fractional a) => [Match a] -> [Match a]
coalesce [] = []
coalesce ms = ms6
    where
        ms1 = zip ms [0..]
        ms2 = [(tick d, tick b, i) | (m,i) <- ms1
                                   , let b = snd m !! 2
                                   , let d = snd m !! 4 ]
        ms3 = nubBy eqDB (sortBy cmp  ms2)
        ms4 = nubBy eqD  (sortBy cmp2 ms3)
        ms5 = sort (map (\(d,b,i) -> i) ms4)
        ms6 = [ ms !! i | i <- ms5]
        eqD   (td, tb, i) (td', tb', i') = td == td'
        eqDB  (td, tb, i) (td', tb', i') = (td, tb) == (td', tb')
        cmp   (td, tb, i) (td', tb', i') = compare (td,tb) (td',tb')

        -- Sort by identical D points, then sort by descending B-D length.
        -- cmp2  (td, tb, i) (td', tb', i') = if cmpd == EQ then cmpbd else cmpd
        --     where cmpd  = compare td td'
        --           cmpbd = compare ((abs (td' - tb')) (abs (td - tb)))
        cmp2  (td, tb, i) (td', tb', i') = 
            if td /= td'
                then compare td td'
                else compare db' db
          where db' = abs (td' - tb')
                db  = abs (td  - tb)

minPipsXA :: (Ord a, Num a) => a -> [PriceBar a] -> Bool
minPipsXA m (x:a:_:_:_:[])  =  m <= abs (pipsDiff x a)
minPipsXA _ _ = False

minPipsAB :: (Ord a, Num a) => a -> [PriceBar a] -> Bool
minPipsAB m (_:a:b:_:_:[])  =  m <= abs (pipsDiff a b)
minPipsAB _ _ = False

minPipsBC :: (Ord a, Num a) => a -> [PriceBar a] -> Bool
minPipsBC m (_:_:b:c:_:[])  =  m <= abs (pipsDiff b c)
minPipsBC _ _ = False

minPipsCD :: (Ord a, Num a) => a -> [PriceBar a] -> Bool
minPipsCD m (_:_:_:c:d:[])  =  m <= abs (pipsDiff c d)
minPipsCD _ _ = False

minTimeXD :: (Ord a, Num a) => Int -> [PriceBar a] -> Bool
minTimeXD m (x:_:_:_:d:[])  =  m <= timeDiff x d
minTimeXD _ _ = False

maxTimeXD :: (Ord a, Num a) => Int -> [PriceBar a] -> Bool
maxTimeXD m (x:_:_:_:d:[])  =  m >= timeDiff x d
maxTimeXD _ _ = False

-- |Given three pricebars, U, V, and W, decide whether the
-- ratio of pip differences VW : UV is among a list of target values,
-- within a given tolerance.
pips_VW_UV :: (Ord a, Fractional a, Show a)
          => [a]            -- ^The target ratios
          ->  a             -- ^The allowed deviation from the target
          -> PriceBar a     -- ^The first pricebar (U)
          -> PriceBar a     -- ^The second pricebar (V)
          -> PriceBar a     -- ^The third pricebar (W)
          -> Bool           -- ^True if VW / UV is close enough to the target
pips_VW_UV rs eps u v w =  closeToOneOf ((-1) * vw / uv) rs eps
    where
        uv = pipsDiff u v
        vw = pipsDiff v w

-- |Given three pricebars, U, V, and W, decide whether the
-- ratio of time differences UV : UW is among a list of target values,
-- within a given tolerance.
time_UV_UW :: (Ord a, Fractional a, Show a)
           => [a]            -- ^The list of target ratios
           ->  a             -- ^The allowed deviation from the target
           -> PriceBar a     -- ^The first pricebar (U)
           -> PriceBar a     -- ^The second pricebar (V)
           -> PriceBar a     -- ^The third pricebar (W)
           -> Bool           -- ^True if UV / UW is close enough to the target
time_UV_UW [] _ _ _ _ = False
time_UV_UW rs eps u v w = closeToOneOf (uv / uw) rs eps
    where
        uv = fromIntegral (timeDiff u v)
        uw = fromIntegral (timeDiff u w)

pips_XA_AB :: (Ord a, Fractional a, Show a) => [a] -> a -> [PriceBar a] -> Bool
pips_XA_AB rs eps (x:a:b:_:_:[]) = pips_VW_UV rs eps x a b
pips_XA_AB _ _ _ = False

pips_AB_BC :: (Ord a, Fractional a, Show a) => [a] -> a -> [PriceBar a] -> Bool
pips_AB_BC rs eps (_:a:b:c:_:[]) = pips_VW_UV rs eps a b c
pips_AB_BC _ _ _ = False

pips_BC_CD :: (Ord a, Fractional a, Show a) => [a] -> a -> [PriceBar a] -> Bool
pips_BC_CD rs eps (_:_:b:c:d:[]) = pips_VW_UV rs eps b c d
pips_BC_CD _ _ _ = False

pips_XA_AD :: (Ord a, Fractional a, Show a) => [a] -> a -> [PriceBar a] -> Bool
pips_XA_AD rs eps (x:a:_:_:d:[]) = pips_VW_UV rs eps x a d
pips_XA_AD _ _ _ = False

time_XB_XD :: (Ord a, Fractional a, Show a) => [a] -> a -> [PriceBar a] -> Bool
time_XB_XD rs eps (x:_:b:_:d:[]) = time_UV_UW rs eps x b d
time_XB_XD _ _ _ = False

harmonic :: (Ord a, Fractional a, Show a)
         =>  String         -- ^Pattern name
         ->  Int            -- ^Number of pricebars in pattern
         ->  a              -- ^Minimum pips for XA leg
         ->  Int            -- ^Minimum ticks for XD
         ->  Int            -- ^Maximum ticks for XD
         -> [a]             -- ^AB/XA pip ratios
         ->  a              -- ^Percent accuracy for AB/XA
         -> [a]             -- ^BC/AB pip ratios
         ->  a              -- ^Percent accuracy for BC/AB
         -> [a]             -- ^CD/BC pip ratios
         ->  a              -- ^Percent accuracy for CD/BC
         -> [a]             -- ^AD/XA pip ratios
         ->  a              -- ^Percent accuracy for AD/XA
         -> [a]             -- ^XB/XD tick ratios
         ->  a              -- ^Percent accuracy for XB/XD
         -> Pattern a
harmonic nm nPbs
         pm tm tM
         pxab pxabE pabc pabcE pbcd pbcdE pxad pxadE
         txbxd txbxdE
    = Pn    nm nPbs

            -- Hard constraints
            [
                --  minPipsXA pm
                  minTimeXD tm
                , maxTimeXD tM
                -- , oppositePeaks
                , pips_XA_AB pxab  pxabE
                , pips_XA_AD pxad  pxadE
                , pips_AB_BC pabc  pabcE
                , pips_BC_CD pbcd  pbcdE
                , time_XB_XD txbxd txbxdE
            ]
            
            -- Soft constraints
            [
            ]

gartley :: (Ord a, Fractional a, Show a) => Pattern a
gartley = harmonic "gartley" 5
                100                         -- min pips XA
                50 200                      -- min and max time XD
                [0.618]             0.05    -- AB/XA pips
                [0.382, 0.886]      0.08    -- BC/AB pips
                [1.270, 1.618]      0.08    -- CD/BC pips
                [0.786]             0.05    -- AD/XA pips
                [0.414, 0.5, 0.618] 0.13    -- XB/XD time

bat :: (Ord a, Fractional a, Show a) => Pattern a
bat = harmonic "bat" 5
                50                          -- min pips XA
                50 200                      -- min and max time XD
                [0.382, 0.50]       0.05    -- AB/XA pips
                [0.382, 0.886]      0.08    -- BC/AB pips
                [1.618, 2.618]      0.08    -- CD/BC pips
                [0.886]             0.05    -- AD/XA pips
                [0.414, 0.5, 0.618] 0.13    -- XB/XD time

butterfly :: (Ord a, Fractional a, Show a) => Pattern a
butterfly = harmonic "butterfly" 5
                50                          -- min pips XA
                50 200                      -- min and max time XD
                [0.786]             0.05    -- AB/XA pips
                [0.382, 0.886]      0.08    -- BC/AB pips
                [1.618, 2.618]      0.08    -- CD/BC pips
                [1.270, 1.618]      0.05    -- AD/XA pips
                [0.414, 0.5, 0.618] 0.13    -- XB/XD time

crab :: (Ord a, Fractional a, Show a) => Pattern a
crab = harmonic "crab" 5
                50                          -- min pips XA
                50 200                      -- min and max time XD
                [0.382, 0.618]      0.05    -- AB/XA pips
                [0.382, 0.886]      0.08    -- BC/AB pips
                [2.240, 3.618]      0.08    -- CD/BC pips
                [1.618]             0.05    -- AD/XA pips
                [0.414, 0.5, 0.618] 0.13    -- XB/XD time

acceptRwRk :: Price -> Price -> Price -> Match Price -> Bool
acceptRwRk cfgRisk cfgRwPct cfgRwrk m = 
    rewardRisk cfgRisk cfgRwPct (snd m) >= cfgRwrk

rewardRisk :: Price -> Price -> [PriceBar Price] -> Price
rewardRisk cfgRisk cfgRwPct m = rw / rk
    where
        (rk, _, _) = risk   cfgRisk  m
        (rw, _, _) = reward cfgRwPct m

risk :: (Ord a, Num a) => a -> [PriceBar a] -> (a, a, Bool)
risk rk m = (rk, rkpr, long)
    where
        rkpr = if peak d
                then high d + rk
                else low d - rk
        d = m !! 4
        long = peak d

reward :: (Ord a, Fractional a) => a -> [PriceBar a] -> (a, a, Bool)
reward pct m = (rw', rwpr, long)
    where
        rw   = if peak d
                then high d - high b
                else low b - low d
        rw' = abs (rw * pct)
        rwpr = if peak d
                then high d - rw'
                else low d + rw'
        b = m !! 2
        d = m !! 4
        long = not (peak d)

