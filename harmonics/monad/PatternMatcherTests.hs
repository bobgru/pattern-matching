module Main where
import PatternMatcher
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

-- TODO
--  validate new price range on match
pureTests = [
        ("test_bullBatInit", test_bullBatInit)
      , ("matchPattern_bullBatX_Yes", matchPattern_bullBatX_Yes)
      , ("matchPattern_bullBatX_No_Concavity", matchPattern_bullBatX_No_Concavity)
      , ("matchPattern_bullBatA_Yes", matchPattern_bullBatA_Yes)
      , ("matchPattern_bullBatA_No_Concavity", matchPattern_bullBatA_No_Concavity)
      , ("matchPattern_bullBatB_Yes", matchPattern_bullBatB_Yes)
      , ("matchPattern_bullBatB_No_Concavity", matchPattern_bullBatB_No_Concavity)
      , ("matchPattern_bullBatB_No_Range", matchPattern_bullBatB_No_Range)
      , ("matchPattern_bullBatC_Yes", matchPattern_bullBatC_Yes)
      , ("matchPattern_bullBatC_No_Concavity", matchPattern_bullBatC_No_Concavity)
      , ("matchPattern_bullBatC_No_Range", matchPattern_bullBatC_No_Range)
      , ("matchPattern_bullBatD_Yes", matchPattern_bullBatD_Yes)
      , ("matchPatterns_bullBatX_Yes", matchPatterns_bullBatX_Yes)
      , ("matchPatterns_bullBatXA_Yes", matchPatterns_bullBatXA_Yes)
      , ("matchPatterns_bullBatXAB_Yes", matchPatterns_bullBatXAB_Yes)
      , ("matchPatterns_bullBatXABC_Yes", matchPatterns_bullBatXABC_Yes)
      , ("matchPatterns_bullBatXABCD_Yes", matchPatterns_bullBatXABCD_Yes)
      , ("matchExtrema_bullBat_first_4", matchExtrema_bullBat_first_4)
    ]

ioTests = [
    ]


mkPb d p v = Pb d 0 p p p p
mkExt pb c = Ext pb c

sampleBullBat_Good = [
      mkExt (mkPb "2020-02-01" 10.0 1000) Valley
    , mkExt (mkPb "2020-03-01" 20.0 1000) Peak
    , mkExt (mkPb "2020-04-01" 15.94 1000) Valley
    , mkExt (mkPb "2020-05-01" 19.16 1000) Peak
    , mkExt (mkPb "2020-06-01" 12.14 1000) Valley
    , mkExt (mkPb "2020-05-01" 15.00 1000) Peak
    , mkExt (mkPb "2020-02-01" 10.0 1000) Valley    -- repeat pattern
    , mkExt (mkPb "2020-03-01" 20.0 1000) Peak
    , mkExt (mkPb "2020-04-01" 15.94 1000) Valley
    , mkExt (mkPb "2020-05-01" 19.16 1000) Peak
    , mkExt (mkPb "2020-06-01" 11.14 1000) Valley
    ]

sampleBullBat_Bad_B = [
      mkExt (mkPb "2020-02-01" 10.0 1000) Valley
    , mkExt (mkPb "2020-03-01" 20.0 1000) Peak
    , mkExt (mkPb "2020-04-01" 16.9 1000) Valley
    , mkExt (mkPb "2020-05-01" 18.5 1000) Peak
    , mkExt (mkPb "2020-06-01" 14.7 1000) Valley
    ]


test_bullBatInit = pass
    where
        m_X = bullBatInit
        pass = case m_X of
            (Pattern [] (PriceRange 0 0) _) -> True
            otherwise -> False

matchPattern_bullBatX_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        pX = bullBatInit
        pass = case matchPattern x pX of
            (Just (Pattern [x] _ _), Nothing) -> True
            otherwise -> False

matchPattern_bullBatX_No_Concavity = pass
    where
        -- Give a Peak as starting point, which is invalid.
        x = sampleBullBat_Good !! 1
        pX = bullBatInit
        pass = case matchPattern x pX of
            (Nothing, Nothing) -> True
            otherwise -> False

matchPattern_bullBatA_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        pass = case matchPattern a pA of
            (Just (Pattern [a,x] _ _), Nothing) -> True
            otherwise -> False

matchPattern_bullBatA_No_Concavity = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 2 -- Valley, invalid for point A
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        pass = case matchPattern a pA of
            (Nothing, Nothing) -> True
            otherwise -> False

matchPattern_bullBatB_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        pass = case matchPattern b pB of
            (Just (Pattern [b,a,x] _ _), Nothing) -> True
            otherwise -> False

matchPattern_bullBatB_No_Concavity = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 3 -- Peak, invalid for point B
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        pass = case matchPattern b pB of
            (Nothing, Nothing) -> True
            otherwise -> False

matchPattern_bullBatB_No_Range = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 0 -- Too low
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        pass = case matchPattern b pB of
            (Nothing, Nothing) -> True
            otherwise -> False

matchPattern_bullBatC_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 3
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        (Just pC, _) = matchPattern b pB
        pass = case matchPattern c pC of
            (Just (Pattern [c,b,a,x] _ _), Nothing) -> True
            otherwise -> False

matchPattern_bullBatD_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 3
        d = sampleBullBat_Good !! 4
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        (Just pC, _) = matchPattern b pB
        (Just pD, _) = matchPattern c pC
        pass = case matchPattern d pD of
            (Nothing, Just (PRZ [d,c,b,a,x] _)) -> True
            otherwise -> False

matchPattern_bullBatC_No_Concavity = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 0 -- Valley, invalid for point C
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        (Just pC, _) = matchPattern b pB
        pass = case matchPattern c pC of
            (Nothing, Nothing) -> True
            otherwise -> False

matchPattern_bullBatC_No_Range = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 1 -- Too high
        pX = bullBatInit
        (Just pA, _) = matchPattern x pX
        (Just pB, _) = matchPattern a pA
        (Just pC, _) = matchPattern b pB
        pass = case matchPattern c pC of
            (Nothing, Nothing) -> True
            otherwise -> False

matchPatterns_bullBatX_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        pX = bullBatInit
        pns = [pX]
        pass = case matchPatterns pns x of
            (_,(x:xs)) -> False                 -- PRZ added
            ([pA],_) -> case pA of
                (Pattern [x] _ _) -> True
                otherwise -> False
            otherwise -> False

matchPatterns_bullBatXA_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        pX = bullBatInit
        pns0 = [pX]
        (pns0', _) = matchPatterns pns0 x
        pns1 = pns0 ++ pns0'
        pass = case matchPatterns pns1 a of
            (_,(x:xs)) -> False                 -- PRZ added
            ([pB],_) -> case pB of
                (Pattern [a,x] _ _) -> True
                otherwise -> False
            otherwise -> False

matchPatterns_bullBatXAB_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        pX = bullBatInit
        pns0 = [pX]
        (pns0', _) = matchPatterns pns0 x
        pns1 = pns0 ++ pns0'
        (pns1', _) = matchPatterns pns1 a
        pns2 = pns1 ++ pns1'
        pass = case matchPatterns pns2 b of
            (_,(x:xs)) -> False                 -- PRZ added
            ([pA',pC],_) -> case (pA',pC) of
                ((Pattern [b'] _ _), (Pattern [b,a,x] _ _)) -> b == b'
                otherwise -> False
            otherwise -> False

matchPatterns_bullBatXABC_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 3
        pX = bullBatInit
        pns0 = [pX]
        (pns0', _) = matchPatterns pns0 x
        pns1 = pns0 ++ pns0'
        (pns1', _) = matchPatterns pns1 a
        pns2 = pns1 ++ pns1'
        (pns2', _) = matchPatterns pns2 b
        pns3 = pns2 ++ pns2'
        pass = case matchPatterns pns3 c of
            ([p1,p2,p3],[]) -> case (p1, p2, p3) of
                ((Pattern [c_1,x_1] _ _), 
                 (Pattern [c_2,b_2] _ _), 
                 (Pattern [c_3,b_3,a_3,x_3] _ _)) -> 
                    (c == c_1) && (c == c_2) && (c == c_3) && 
                    (b == b_2) && (b == b_3) && 
                    (a == a_3) &&
                    (x == x_1) && (x == x_3)
                otherwise -> False
            otherwise -> False

matchPatterns_bullBatXABCD_Yes = pass
    where
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 3
        d = sampleBullBat_Good !! 4
        pX = bullBatInit
        pns0 = [pX]
        (pns0', _) = matchPatterns pns0 x
        pns1 = pns0 ++ pns0'
        (pns1', _) = matchPatterns pns1 a
        pns2 = pns1 ++ pns1'
        (pns2', _) = matchPatterns pns2 b
        pns3 = pns2 ++ pns2'
        (pns3', _) = matchPatterns pns3 c
        pns4 = pns3 ++ pns3'
        pass = case matchPatterns pns4 d of
            ([p1],[prz]) -> case (p1, prz) of
                ((Pattern [d_1] _ _), 
                 (PRZ [d_2,c_2,b_2,a_2,x_2] _)) ->
                    (d == d_1) && (d == d_2) &&
                    (c == c_2) &&
                    (b == b_2) &&
                    (a == a_2) &&
                    (x == x_2)
                otherwise -> False
            otherwise -> False


matchExtrema_bullBat_first_4 = pass
    where
        pms = initPatternMatcherState pns
        pns = [bullBatInit]
        ((), pms') = runPatternMatcher (matchExtrema (take 4 sampleBullBat_Good)) pms
        pns' = getPatterns pms'
        przs' = getPRZs pms'
        counts_ok = length pns' == 8 && length przs' == 0
        -- The 8 patterns are:
        -- (), (x), (xa), (b), (xab), (xc), (bc), (xabc)
        x = sampleBullBat_Good !! 0
        a = sampleBullBat_Good !! 1
        b = sampleBullBat_Good !! 2
        c = sampleBullBat_Good !! 3
        p0 = []                         -- pattern starter
        p1 = [x]                        -- first extremum
        p2 = [a,x]                      -- second extremum
        p3 = [b]; p4 = [b,a,x]          -- third extremum
        p5 = [c,x]; p6 = [c,b]; p7 = [c,b,a,x]  -- fourth extremum
        pts_ok = comparePatternPoints pns' [p0,p1,p2,p3,p4,p5,p6,p7]
        pass = counts_ok && pts_ok
        
        comparePatternPoints pns pts
            | null pns || (length pns /= length pts) = False
            | otherwise =
                let pts' = map points pns
                in pts' == pts    
