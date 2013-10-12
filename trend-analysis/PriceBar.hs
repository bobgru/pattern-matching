module PriceBar where

import CSV
import Graphics.Rendering.OpenGL

type Price = GLfloat

data PriceRange = PriceRange Price Price  deriving (Show, Eq)   -- low, high

data PriceBar = PriceBar {
    date     :: String,
    open     :: Price,
    high     :: Price,
    low      :: Price,
    close    :: Price,
    volume   :: Integer,
    adjClose :: Price
} deriving (Show, Eq)



data Concavity = Peak | Valley | Both deriving (Show, Eq)
data Extremum = Extremum { priceBar::PriceBar, concavity::Concavity }
                    deriving (Show, Eq)

parsePriceBars' :: IO String -> IO [[String]]
parsePriceBars' getInput = do
    c <- getInput
    case parseCSV c of
        Left e  -> let s = "Error parsing input: " ++ (show e)
                   in error s
        Right r -> return r

parsePriceBars_stdin :: IO [[String]]
parsePriceBars_stdin = parsePriceBars' getContents

parsePriceBars :: FilePath -> IO [[String]]
parsePriceBars path = parsePriceBars' (readFile path)

pbsFromFile :: FilePath -> IO [PriceBar]
pbsFromFile path = do
    ss <- parsePriceBars path
    if null ss
        then return []
        else
            if isFx (head ss)
                then
                    return (map fxpbFromText (tail ss)) -- TODO assumes header row
                else
                    return (map pbFromText (tail ss)) -- TODO assumes header row

isFx :: [String] -> Bool
isFx ("<TICKER>":_) = True
isFx _ = False

pbFromText :: [String] -> PriceBar
pbFromText (d:o:h:l:c:v:a:[]) = PriceBar d o' h' l' c' v' a'
    where
        o' = getp o
        h' = getp h
        l' = getp l
        c' = getp c
        v' = fromIntegral (geti v)
        a' = getp a
pbFromText ss = error ("Invalid string data for price bar:"++ (show ss))

fxpbFromText :: [String] -> PriceBar
fxpbFromText (s:d:t:o:l:h:c:[]) = PriceBar d o' h' l' c' 0 0
    where
        o' = getp o
        h' = getp h
        l' = getp l
        c' = getp c
fxpbFromText ss = error ("Invalid string data for price bar:"++ (show ss))


--TODO handle parse errors (i.e. map to 0)
getp :: String -> Price
getp s = (dollars * 1.0) + (cents / (10 ^ (length c_str)))
    where
        d_str = takeWhile (\c -> c /= '.') s
        c_str = drop ((length d_str) + 1) s
        dollars = fromIntegral (geti d_str)
        cents = fromIntegral (geti c_str)

geti :: String -> Int
geti s = read s 


priceBarsToTriplets :: [PriceBar] -> [(PriceBar,PriceBar,PriceBar)]
priceBarsToTriplets pbs = zip3 pbs (tail pbs) ((tail.tail) pbs)

inRange :: Price -> Price -> Price -> Bool
inRange low high test_val = test_val >= low && test_val <= high

doesPriceBarIntersectRange :: PriceRange -> PriceBar -> Bool
doesPriceBarIntersectRange (PriceRange lo hi) pb = (high pb > lo) && (low pb < hi)

isPriceBarInRange :: PriceRange -> (PriceBar -> Price) -> PriceBar -> Bool
isPriceBarInRange (PriceRange lo hi) getPrice pb = inRange lo hi (getPrice pb)

highestHigh pbs = maximum (map high pbs)
lowestLow pbs = minimum (map low pbs)

isBullish :: PriceBar -> Bool
isBullish (PriceBar _ o _ _ c _ _) = c > o

isBearish = not . isBullish

isPeak :: PriceBar -> PriceBar -> PriceBar -> Bool
isPeak pb1 pb2 pb3 = high pb2 > high pb1 && high pb2 > high pb3

isValley :: PriceBar -> PriceBar -> PriceBar -> Bool
isValley pb1 pb2 pb3 = low pb2 < low pb1 && low pb2 < low pb3

isPeakInRange :: PriceRange -> Extremum -> Bool
isPeakInRange pr (Extremum _ Valley) = error "isPeakInRange received a valley"
isPeakInRange pr (Extremum pb _) = isPriceBarInRange pr (\x -> high x) pb

isValleyInRange :: PriceRange -> Extremum -> Bool
isValleyInRange pr (Extremum _ Peak) = error "isValleyInRange received a peak"
isValleyInRange pr (Extremum pb _) = isPriceBarInRange pr (\x -> low x) pb

isExtremumInRange :: PriceRange -> Extremum -> Bool
isExtremumInRange pr x@(Extremum pb Peak) = isPeakInRange pr x
isExtremumInRange pr x@(Extremum pb Valley) = isValleyInRange pr x
isExtremumInRange pr x@(Extremum pb Both) = (isPeakInRange pr x) || (isValleyInRange pr x)

getExtremum :: PriceBar -> PriceBar -> PriceBar -> Maybe Extremum
getExtremum t1 t2 t3 =
    let lo1 = low t1; hi1 = high t1
        lo2 = low t2; hi2 = high t2
        lo3 = low t3; hi3 = high t3
        
        peak   = (hi1 < hi2) && (hi2 > hi3)
        valley = (lo1 > lo2) && (lo2 < lo3)
        isExtremum = peak || valley
    in 
        if isExtremum
            then
                if peak 
                    then Just (Extremum t2 Peak)
                    else Just (Extremum t2 Valley)
            else Nothing

getExtrema :: [PriceBar] -> [Extremum]
getExtrema (t1:t2:t3:ts) =
    case getExtremum t1 t2 t3 of
        Nothing -> getExtrema (t2:t3:ts)
        Just e -> e : (getExtrema (t2:t3:ts))
getExtrema _ = []

addExtremum :: [PriceBar] -> [Extremum] -> PriceBar -> [Extremum]
addExtremum [] _ _ = []         -- not enough PriceBars
addExtremum [t] _ _ = []        -- not enough PriceBars
addExtremum ts es t3 = es ++ (getExtrema (t1:(t2:[t3])))
    where
        t1 = last (init ts)
        t2 = last ts

exHi :: Extremum -> Price
exHi x = high (priceBar x)

exLo :: Extremum -> Price
exLo x = low (priceBar x)
