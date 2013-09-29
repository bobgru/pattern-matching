import PriceBar
import TestFramework

main = putStrLn (runTests tests)

tests = [
      ("successTest", successTest)
--    , ("failTest", failTest)
    , ("createPriceBarTest", createPriceBarTest)
    , ("createExtremum", createExtremum)
    , ("createExtremaFromRise", createExtremaFromRise)
    , ("createExtremaFromFall", createExtremaFromFall)
    , ("createExtremaFromPeak", createExtremaFromPeak)
    , ("createExtremaFromValley", createExtremaFromValley)
    , ("createExtremaFromSmallStream", createExtremaFromSmallStream)
    , ("addExtremum_None", addExtremum_None)
    , ("addExtremum_One", addExtremum_One)
    , ("addExtremum_Two", addExtremum_Two)
    , ("addExtremum_Many", addExtremum_Many)
    , ("inRange_Yes", inRange_Yes)
    , ("inRange_No_Low", inRange_No_Low)
    , ("inRange_No_High", inRange_No_High)
    , ("isPriceBarInRange_Low_Yes", isPriceBarInRange_Low_Yes)
    , ("isPriceBarInRange_High_Yes", isPriceBarInRange_High_Yes)
    , ("isPriceBarInRange_Low_No_Low", isPriceBarInRange_Low_No_Low)
    , ("isPriceBarInRange_Low_No_High", isPriceBarInRange_Low_No_High)
    , ("isPriceBarInRange_High_No_Low", isPriceBarInRange_High_No_Low)
    , ("isPriceBarInRange_High_No_High", isPriceBarInRange_High_No_High)
    , ("isExtremumInRange_Valley_Yes", isExtremumInRange_Valley_Yes)
    , ("isExtremumInRange_Valley_No_Low", isExtremumInRange_Valley_No_Low)
    , ("isExtremumInRange_Valley_No_High", isExtremumInRange_Valley_No_High)
    , ("isExtremumInRange_Peak_Yes", isExtremumInRange_Peak_Yes)
    , ("isExtremumInRange_Peak_No_Low", isExtremumInRange_Peak_No_Low)
    , ("isExtremumInRange_Peak_No_High", isExtremumInRange_Peak_No_High)
    , ("isExtremumInRange_Both_Yes_Valley", isExtremumInRange_Both_Yes_Valley)
    , ("isExtremumInRange_Both_Yes_Peak", isExtremumInRange_Both_Yes_Peak)
    , ("priceBarsToTriplets_test", priceBarsToTriplets_test)
    , ("isPeak_Yes", isPeak_Yes)
    , ("isPeak_No_Valley", isPeak_No_Valley)
    , ("isPeak_No_Rise", isPeak_No_Rise)
    , ("isValley_Yes", isValley_Yes)
    , ("isValley_No_Peak", isValley_No_Peak)
    , ("isValley_No_Rise", isValley_No_Rise)
    , ("doesPriceBarIntersectRange_Yes_OverlapRangeHigh", 
            doesPriceBarIntersectRange_Yes_OverlapRangeHigh)
    , ("doesPriceBarIntersectRange_Yes_OverlapRangeLow",
            doesPriceBarIntersectRange_Yes_OverlapRangeLow)
    , ("doesPriceBarIntersectRange_Yes_OverlapRangeBoth",
            doesPriceBarIntersectRange_Yes_OverlapRangeBoth)
    , ("doesPriceBarIntersectRange_Yes_OverlapRangeNeither",
            doesPriceBarIntersectRange_Yes_OverlapRangeNeither)
    , ("doesPriceBarIntersectRange_No_Low", doesPriceBarIntersectRange_No_Low)
    , ("doesPriceBarIntersectRange_No_High", doesPriceBarIntersectRange_No_High)
    ]

successTest = True
failTest = False

samplePriceBar = Pb "2011-05-24" 0 1317.70 1323.72 1313.87 1316.28

sampleFall = reverse [
        Pb "2011-05-23" 0 1333.07 1333.07 1312.88 1317.37,
        Pb "2011-05-20" 1 1342.00 1342.00 1330.67 1333.27,
        Pb "2011-05-19" 2 1342.40 1346.82 1336.36 1343.60 
    ]

sampleRise = reverse [
        Pb "2011-05-19" 0 1342.40 1346.82 1336.36 1343.60,
        Pb "2011-05-18" 1 1328.54 1341.82 1326.59 1340.68,
        Pb "2011-05-17" 2 1326.10 1330.42 1318.51 1328.98
    ]

samplePeak = reverse [
        Pb "2011-05-20" 0 1342.00 1342.00 1330.67 1333.27,
        Pb "2011-05-19" 1 1342.40 1346.82 1336.36 1343.60,
        Pb "2011-05-18" 2 1328.54 1341.82 1326.59 1340.68
    ]

sampleValley = reverse [
    Pb "2011-05-18" 0 1328.54 1341.82 1326.59 1340.68,
    Pb "2011-05-17" 1 1326.10 1330.42 1318.51 1328.98,
    Pb "2011-05-16" 2 1334.77 1343.33 1327.32 1329.47
    ]

sampleSmallStream = reverse [
    Pb "2011-05-23" 0 1333.07 1333.07 1312.88 1317.37,
    Pb "2011-05-20" 1 1342.00 1342.00 1330.67 1333.27,
    Pb "2011-05-19" 2 1342.40 1346.82 1336.36 1343.60,
    Pb "2011-05-18" 3 1328.54 1341.82 1326.59 1340.68,
    Pb "2011-05-17" 4 1326.10 1330.42 1318.51 1328.98,
    Pb "2011-05-16" 5 1334.77 1343.33 1327.32 1329.47,
    Pb "2011-05-13" 6 1348.69 1350.47 1333.36 1337.77,
    Pb "2011-05-12" 7 1339.39 1351.05 1332.03 1348.65,
    Pb "2011-05-11" 8 1354.51 1354.51 1336.36 1342.08,
    Pb "2011-05-10" 9 1348.34 1359.44 1348.34 1357.16
    ]


createPriceBarTest :: Bool
createPriceBarTest = 
    let priceBar = samplePriceBar
    in True

createExtremum :: Bool
createExtremum =
    let pb = samplePriceBar
        e = Ext pb Peak
    in True

createExtremaFromRise = pass
    where 
        es = getExtrema sampleRise
        pass = null es

createExtremaFromFall = pass
    where 
        es = getExtrema sampleFall
        pass = null es

createExtremaFromPeak = pass
    where 
        es = getExtrema samplePeak
        pass = (length es) == 1

createExtremaFromValley = pass
    where 
        es = getExtrema sampleValley
        pass = (length es) == 1

createExtremaFromSmallStream = pass
    where 
        es = getExtrema sampleSmallStream
        pass = (length es) == 3

addExtremum_None = pass
    where
        es = addExtremum [] [] samplePriceBar
        pass = null es
        
addExtremum_One = pass
    where
        es = addExtremum [head sampleSmallStream] [] (sampleSmallStream!!1)
        pass = null es
        
addExtremum_Two = pass
    where
        es = addExtremum (take 3 sampleSmallStream) [] (sampleSmallStream!!3)
        pass = (length es) == 1

addExtremum_Many = pass
    where
        pbs = init sampleSmallStream
        es = getExtrema pbs
        es' = addExtremum pbs es (last sampleSmallStream)
        pass = (length es') == 3

inRange_Yes = pass
    where
        pass = inRange 120.0 130.0 123.45 

inRange_No_Low = pass
    where
        pass = not (inRange 120.0 130.0 119.99)

inRange_No_High = pass
    where
        pass = not (inRange 120.0 130.0 130.1)

isPriceBarInRange_Low_Yes = pass
    where
        pass = isPriceBarInRange (PriceRange 120.0 130.0) (\x -> low x) pb
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isPriceBarInRange_High_Yes = pass
    where
        pass = isPriceBarInRange (PriceRange 130.0 140.0) (\x -> high x) pb
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isPriceBarInRange_Low_No_Low = pass
    where
        pass = not (isPriceBarInRange (PriceRange 130.0 140.0) (\x -> low x) pb)
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isPriceBarInRange_Low_No_High = pass
    where
        pass = not (isPriceBarInRange (PriceRange 110.0 120.0) (\x -> low x) pb)
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isPriceBarInRange_High_No_Low = pass
    where
        pass = not (isPriceBarInRange (PriceRange 140.0 150.0) (\x -> high x) pb)
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isPriceBarInRange_High_No_High = pass
    where
        pass = not (isPriceBarInRange (PriceRange 120.0 130.0) (\x -> high x) pb)
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Valley_Yes = pass
    where
        pass = isExtremumInRange (PriceRange 120.0 130.0) ext
        ext = Ext pb Valley
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Valley_No_Low = pass
    where
        pass = not (isExtremumInRange (PriceRange 130.0 140.0) ext)
        ext = Ext pb Valley
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Valley_No_High = pass
    where
        pass = not (isExtremumInRange (PriceRange 110.0 120.0) ext)
        ext = Ext pb Valley
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Peak_Yes = pass
    where
        pass = isExtremumInRange (PriceRange 130.0 140.0) ext
        ext = Ext pb Peak
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Peak_No_Low = pass
    where
        pass = not (isExtremumInRange (PriceRange 140.0 150.0) ext)
        ext = Ext pb Peak
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Peak_No_High = pass
    where
        pass = not (isExtremumInRange (PriceRange 120.0 130.0) ext)
        ext = Ext pb Peak
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Both_Yes_Valley = pass
    where
        pass = isExtremumInRange (PriceRange 120.0 130.0) ext
        ext = Ext pb Both
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

isExtremumInRange_Both_Yes_Peak = pass
    where
        pass = isExtremumInRange (PriceRange 130.0 140.0) ext
        ext = Ext pb Both
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

priceBarsToTriplets_test = pass
    where
        pbs = sampleSmallStream
        pb3s = priceBarsToTriplets pbs
        pass = (length pb3s == length pbs - 2)
        
isPeak_Yes = pass
    where
        pbs = samplePeak
        pass = isPeak (pbs!!0) (pbs!!1) (pbs!!2)

isPeak_No_Valley = pass
    where
        pbs = sampleValley
        pass = not (isPeak (pbs!!0) (pbs!!1) (pbs!!2))

isPeak_No_Rise = pass
    where
        pbs = sampleRise
        pass = not (isPeak (pbs!!0) (pbs!!1) (pbs!!2))
        
isValley_Yes = pass
    where
        pbs = sampleValley
        pass = isValley (pbs!!0) (pbs!!1) (pbs!!2)

isValley_No_Peak = pass
    where
        pbs = samplePeak
        pass = not (isValley (pbs!!0) (pbs!!1) (pbs!!2))

isValley_No_Rise = pass
    where
        pbs = sampleRise
        pass = not (isValley (pbs!!0) (pbs!!1) (pbs!!2))

doesPriceBarIntersectRange_Yes_OverlapRangeLow = pass
    where
        pass = doesPriceBarIntersectRange (PriceRange 130.0 140.0) pb
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

doesPriceBarIntersectRange_Yes_OverlapRangeHigh = pass
    where
        pass = doesPriceBarIntersectRange (PriceRange 120.0 130.0) pb
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

doesPriceBarIntersectRange_Yes_OverlapRangeBoth = pass
    where
        pass = doesPriceBarIntersectRange (PriceRange 127.0 130.0) pb
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

doesPriceBarIntersectRange_Yes_OverlapRangeNeither = pass
    where
        pass = doesPriceBarIntersectRange (PriceRange 120.0 140.0) pb
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

doesPriceBarIntersectRange_No_Low = pass
    where
        pass = not (doesPriceBarIntersectRange (PriceRange 140.0 150.0) pb)
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

doesPriceBarIntersectRange_No_High = pass
    where
        pass = not (doesPriceBarIntersectRange (PriceRange 110.0 120.0) pb)
        pb = Pb "2020-01-01" 10 127.0 135.0 125.0 131.0

