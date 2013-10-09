import Harmonics
import PriceBar
import TestFramework

main = do
    let testResults = runTests pureTests
    putStrLn testResults
    
    ioTestResults <- runTestsIO ioTests
    putStrLn ioTestResults

pureTests = [
      ("tailsOfLength_0_of_3",      tailsOfLength_0_of_3)
    , ("tailsOfLength_1_of_3",      tailsOfLength_1_of_3)
    , ("tailsOfLength_2_of_3",      tailsOfLength_2_of_3)
    , ("tailsOfLength_3_of_3",      tailsOfLength_3_of_3)
    , ("tailsOfLength_4_of_3",      tailsOfLength_4_of_3)
    , ("pipsDiff_going_up",         pipsDiff_going_up)
    , ("pipsDiff_going_down",       pipsDiff_going_down)
    , ("pipsDiff_inverted",     pipsDiff_inverted)
    , ("timeDiff_forward",          timeDiff_forward)
    , ("timeDiff_backward",         timeDiff_backward)
    , ("equalWithin_no_low",        equalWithin_no_low)
    , ("equalWithin_no_high",       equalWithin_no_high)
    , ("equalWithin_yes_low",       equalWithin_yes_low)
    , ("equalWithin_yes_high",      equalWithin_yes_high)
    --, ("equalWithin_error_1",       equalWithin_error_1)
    , ("closeToOneOf_none",         closeToOneOf_none)
    , ("closeToOneOf_one_1",        closeToOneOf_one_1)
    , ("closeToOneOf_one_2",        closeToOneOf_one_2)
    , ("closeToOneOf_one_3",        closeToOneOf_one_3)
    , ("oppositePeaks_yes",         oppositePeaks_yes)
    , ("oppositePeaks_non_alt",     oppositePeaks_non_alt)
    , ("oppositePeaks_low_high",    oppositePeaks_low_high)
    , ("extrema_up_smooth_none",    extrema_up_smooth_none)
    , ("extrema_up_jagged_none",    extrema_up_jagged_none)
    , ("extrema_down_smooth_none",  extrema_down_smooth_none)
    , ("extrema_down_jagged_none",  extrema_down_jagged_none)
    , ("extrema_strength_3",        extrema_strength_3)
    , ("extrema_strength_2",        extrema_strength_2)
    , ("extrema_indeterminates",    extrema_indeterminates)
    , ("extOK_same_type",           extOK_same_type)
    , ("extOK_too_close",           extOK_too_close)
    , ("extOK_yes",                 extOK_yes)
    , ("seqs_test_1",               seqs_test_1)
    , ("seqs_test_2",               seqs_test_2)
    , ("seqs_test_3",               seqs_test_3)
    , ("seqs_test_4",               seqs_test_4)
    , ("goodTails_test_4",          goodTails_test_4)
    , ("tailSeqs_test_4",           tailSeqs_test_4)
    , ("seqs_test_5",               seqs_test_5)
    , ("seqs_test_6",               seqs_test_6)
    , ("goodTails_test_6",          goodTails_test_6)
    , ("tailSeqs_test_6a",          tailSeqs_test_6a)
    , ("tailSeqs_test_6b",          tailSeqs_test_6b)
    , ("tailSeqs_test_6c",          tailSeqs_test_6c)
    -- END OF TESTS (command-U to add a test from selected text)
  ]

{-
    match
    matchSoft
    matchCount
    allMatches
    minPipsXA
    minPipsAB
    minPipsBC
    minPipsCD
    minTimeXD
    maxTimeXD
    pips_VW_UV
    time_UV_UW
    pips_XA_AB
    pips_AB_BC
    pips_BC_CD
    pips_XA_AD
    time_XB_XD
-}

ioTests = []

mkPb = makePb ""
mkPb' h l = makePb "" h h l l
mkEx h l t p = 
    Pb { 
          date = ""
        , tick = t
        , ext  = True
        , peak = p
        , open = h
        , high = h
        , low  = l
        , close = l
        , volume = 0
        , adjClose = 0
    }
mkEx' h l = mkEx h l 0

mkExs  = map (uncurry mkPb')
mkExs' = map (\(t,h,l,p) -> mkEx h l t p )

-- Some standard pricebars
pbA = mkPb' 200 100
pbB = mkPb' 250 150
pbC = mkPb' 250 100
pbD = mkPb' 200 150

-- Some standard pricebars with ticks
pbA' = fixupTick 100 pbA
pbB' = fixupTick 200 pbB

-- Some standard 2-bar patterns
--goingUp   = [pbA, pbB]
--goingDown = [pbB, pbA]
--pennant   = [pbC, pbD]
--megaphone = [pbD, pbC]

-- Some lists of pricebars for extrema tests
xs_up_smooth_none   = mkExs [(120,100),(130,110),(140,120),(150,130)]
xs_up_jagged_none   = mkExs [(120,100),(140,120),(130,110),(150,130),(160,140)]
xs_down_smooth_none = reverse xs_up_smooth_none
xs_down_jagged_none = reverse xs_up_jagged_none
xs_strength_2_or_3  = mkExs
    [
        (120,100),(130,110),(140,120),(130,110),(150,130),
        (140,120),(130,110),(140,120),(120,100),(130,110)
    ]
xs_indeterminates   = mkExs
    [
        (120,100),(130,110),(140,120),(150,100),
        (140,120),(130,110),(150,100),(130,110),(120,100)
    ]

-- Some standard extrema
exA  = mkEx 110 100   0  False
exB  = mkEx 200 190 100  True
exB' = mkEx  90  80 100  True        -- low peak
exC  = mkEx 130 120 200  False
exD  = mkEx 190 180 300  True
exE  = mkEx 120 110 400  False

-- Some standard lists of extrema
altExts   = [exA, exB, exC, exD, exE]
naltExts  = [exB, exA, exC, exD, exE]
lohiExts  = [exA, exB', exC, exD, exE]

-- A standard percent tolerance
eps = 0.1

tailsOfLength_0_of_3 = assertAreEqual []            (tailsOfLength 0 [1,2,3])
tailsOfLength_1_of_3 = assertAreEqual [[1],[2],[3]] (tailsOfLength 1 [1,2,3])
tailsOfLength_2_of_3 = assertAreEqual [[1,2],[2,3]] (tailsOfLength 2 [1,2,3])
tailsOfLength_3_of_3 = assertAreEqual [[1,2,3]]     (tailsOfLength 3 [1,2,3])
tailsOfLength_4_of_3 = assertAreEqual []            (tailsOfLength 4 [1,2,3])

pipsDiff_going_up   = assertAreEqual   100      (pipsDiff exA  exB)
pipsDiff_going_down = assertAreEqual  (-80)     (pipsDiff exB  exC)
pipsDiff_inverted   = assertAreEqual    30      (pipsDiff exB' exC)
--pipsDiff_peak_peak  = assertAreEqual 100      (pipsDiff exB exD)
--pipsDiff_val_val    = assertAreEqual 100      (pipsDiff exA exC)

timeDiff_forward    = assertAreEqual 100 (timeDiff pbA' pbB')
timeDiff_backward   = assertAreEqual 100 (timeDiff pbB' pbA')

equalWithin_no_low    = assertAreEqual False (equalWithin eps  90 100)
equalWithin_no_high   = assertAreEqual False (equalWithin eps 110 100)
equalWithin_yes_low   = assertAreEqual True  (equalWithin eps  91 100)
equalWithin_yes_high  = assertAreEqual True  (equalWithin eps 109 100)
equalWithin_error_1   = assertAreEqual False (equalWithin eps 111 0.09)

closeToOneOf_none  = assertAreEqual False (closeToOneOf  45 [50, 100, 200] eps)
closeToOneOf_one_1 = assertAreEqual True  (closeToOneOf  46 [50, 100, 200] eps)
closeToOneOf_one_2 = assertAreEqual True  (closeToOneOf  91 [50, 100, 200] eps)
closeToOneOf_one_3 = assertAreEqual True  (closeToOneOf 181 [50, 100, 200] eps)

oppositePeaks_yes       = assertAreEqual True  (oppositePeaks altExts)
oppositePeaks_non_alt   = assertAreEqual False (oppositePeaks naltExts)
oppositePeaks_low_high  = assertAreEqual False (oppositePeaks lohiExts)

extrema_up_smooth_none    = assertAreEqual [] (extrema 3 xs_up_smooth_none)
extrema_up_jagged_none    = assertAreEqual [] (extrema 3 xs_up_jagged_none)
extrema_down_smooth_none  = assertAreEqual [] (extrema 3 xs_down_smooth_none)
extrema_down_jagged_none  = assertAreEqual [] (extrema 3 xs_down_jagged_none)
extrema_strength_3        =
    assertAreEqual expected (extrema 3 xs_strength_2_or_3)
    where expected = [ 
                          mkEx' 150 130 True
                        , mkEx' 130 110 False
                        , mkEx' 120 100 False
                     ]
extrema_strength_2        =
    assertAreEqual expected (extrema 2 xs_strength_2_or_3)
    where expected = [ 
                          mkEx' 140 120 True
                        , mkEx' 130 110 False
                        , mkEx' 150 130 True
                        , mkEx' 130 110 False
                        , mkEx' 140 120 True
                        , mkEx' 120 100 False
                     ]
extrema_indeterminates  = assertAreEqual [] (extrema 3 xs_indeterminates)

extOK_same_type = assertAreEqual False (extOK  5 x y)
    where x = mkEx' 140 120 True
          y = mkEx' 130 110 True
extOK_too_close = assertAreEqual False (extOK 50 x y)
    where x = mkEx' 140 120 True
          y = mkEx' 130 110 False
extOK_yes       = assertAreEqual True  (extOK 20 x y)
    where x = mkEx' 140 120 True
          y = mkEx' 130 110 False

seqs_test_1 = assertAreEqual expected (seqs 20 2 [a,b,c,d])
    where
        expected = [[ b, c ]]
        a = mkEx 100  90 0 True
        b = mkEx 120 110 1 True
        c = mkEx 100  90 2 False
        d = mkEx 140 130 3 False
seqs_test_2 = assertAreEqual expected (seqs 10 2 [a,b,c,d])
    where
        expected = [ [a,c], [a,d], [b,c], [b,d] ]
        a = mkEx 120 110 0 True
        b = mkEx 120 110 1 True
        c = mkEx 110 100 2 False
        d = mkEx 110 100 3 False
seqs_test_3 = assertAreEqual expected (seqs 10 2 [a,b,c,d,e,f])
    where
        expected = [[a,c], [b,c], [c,e], [c,f], [d,f]]
        a = mkEx 120 110 0 True
        b = mkEx 120 110 1 True
        c = mkEx 110 100 2 False
        d = mkEx 125 115 3 False
        e = mkEx 120 110 4 True
        f = mkEx 135 125 5 True
seqs_test_4 = assertAreEqual expected (seqs 10 3 [a,b,c,d,e,f])
    where
        expected = [[a,c,e], [a,c,f], [b,c,e], [b,c,f]]
        a = mkEx 120 110 0 True
        b = mkEx 120 110 1 True
        c = mkEx 110 100 2 False
        d = mkEx 125 115 3 False
        e = mkEx 120 110 4 True
        f = mkEx 135 125 5 True

goodTails_test_4 = assertAreEqual expected (goodTails 10 a [b,c,d,e,f])
    where
        expected = [ (c, [d,e,f]) ]
        a = mkEx 120 110 0 True
        b = mkEx 120 110 1 True
        c = mkEx 110 100 2 False
        d = mkEx 125 115 3 False
        e = mkEx 120 110 4 True
        f = mkEx 135 125 5 True

tailSeqs_test_4 = assertAreEqual expected (tailSeqs 10 1 (c, [d,e,f]))
    where
        expected = [ [c,e], [c,f] ]
        a = mkEx 120 110 0 True
        b = mkEx 120 110 1 True
        c = mkEx 110 100 2 False
        d = mkEx 125 115 3 False
        e = mkEx 120 110 4 True
        f = mkEx 135 125 5 True

seqs_test_5 = assertAreEqual expected (seqs 10 3 [a,b,c,d,e,f])
    where
        expected = 
            [
                [a,b,c], [a,b,e], [a,d,e], [b,c,d],
                [b,c,f], [b,e,f], [c,d,e], [d,e,f]
            ]
        a = mkEx 120 110 0 True
        b = mkEx 110 100 1 False
        c = mkEx 120 110 2 True
        d = mkEx 110 100 3 False
        e = mkEx 120 110 4 True
        f = mkEx 110 100 5 False

seqs_test_6 = assertAreEqual expected (seqs 10 4 [a,b,c,d,e,f])
    where
        expected = 
            [
                [a,b,c,d], [a,b,c,f], [a,b,e,f], 
                [a,d,e,f], [b,c,d,e], [c,d,e,f]
            ]
        a = mkEx 120 110 0 True
        b = mkEx 110 100 1 False
        c = mkEx 120 110 2 True
        d = mkEx 110 100 3 False
        e = mkEx 120 110 4 True
        f = mkEx 110 100 5 False

goodTails_test_6 = assertAreEqual expected (goodTails 10 a [b,c,d,e,f])
    where
        expected = [ (b, [c,d,e,f]), (d, [e,f]), (f, []) ]
        a = mkEx 120 110 0 True
        b = mkEx 110 100 1 False
        c = mkEx 120 110 2 True
        d = mkEx 110 100 3 False
        e = mkEx 120 110 4 True
        f = mkEx 110 100 5 False

tailSeqs_test_6a = assertAreEqual expected (tailSeqs 10 2 (b, [c,d,e,f]))
    where
        xs       = [ (b, [c,d,e,f]), (d, [e,f]), (f, []) ]
        expected = [ [b,c,d], [b,c,f], [b,e,f] ]
        a = mkEx 120 110 0 True
        b = mkEx 110 100 1 False
        c = mkEx 120 110 2 True
        d = mkEx 110 100 3 False
        e = mkEx 120 110 4 True
        f = mkEx 110 100 5 False

tailSeqs_test_6b = assertAreEqual expected (tailSeqs 10 2 (d, [e,f]))
    where
        expected = [ [d,e,f] ]
        a = mkEx 120 110 0 True
        b = mkEx 110 100 1 False
        c = mkEx 120 110 2 True
        d = mkEx 110 100 3 False
        e = mkEx 120 110 4 True
        f = mkEx 110 100 5 False

tailSeqs_test_6c = assertAreEqual expected (tailSeqs 10 2 (f, []))
    where
        xs       = [ (b, [c,d,e,f]), (d, [e,f]), (f, []) ]
        expected = []
        a = mkEx 120 110 0 True
        b = mkEx 110 100 1 False
        c = mkEx 120 110 2 True
        d = mkEx 110 100 3 False
        e = mkEx 120 110 4 True
        f = mkEx 110 100 5 False
