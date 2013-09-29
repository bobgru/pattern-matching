module PriceBar where

import CSV
import Graphics.Rendering.OpenGL

--type Price = Double
type Price = GLfloat

data PriceRange = PriceRange Price Price  deriving (Show, Eq)   -- low, high

data PriceBar = Pb {
      date     :: String
    , tick     :: Int           -- ^Simple horizontal metric
    , open     :: Price
    , high     :: Price
    , low      :: Price
    , close    :: Price
} deriving (Show, Eq)



data Concavity = Peak | Valley | Both deriving (Show, Eq)
data Extremum = Ext { priceBar::PriceBar, concavity::Concavity }
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

pbsFromFile :: Int -> FilePath -> IO [PriceBar]
pbsFromFile n path = do
    ss <- parsePriceBars path
    let pbs = if null ss
                then []
                else map (pbFromText n) (tail ss) -- TODO assumes header row
    return (timestamp pbs)

isFx :: [String] -> Bool
isFx ("<TICKER>":_) = True
isFx _ = False

timestamp :: [PriceBar] -> [PriceBar]
timestamp pbs = [fixupTick n pb | (n,pb) <- zip [0..] pbs ]

fixupTick :: Int -> PriceBar -> PriceBar
fixupTick n pb = pb { tick = n }

makePb d o h l c = 
    Pb { 
          date = d
        , tick = 0
        --, ext  = False
        --, peak = False
        , open = o
        , high = h
        , low  = l
        , close = c
    }

pbFromText :: Int -> [String] -> PriceBar
pbFromText n ss@(s:d:t:o:l:h:c:[])
    | all isOK [o,l,h,c]  =  makePb d o' h' l' c'
    | otherwise  = error msg
    where
        msg = "Invalid data for price bar:" ++ show ss
        isOK :: String -> Bool
        isOK s = hasPoint s && fracDigitsOK n s
        cvt :: String -> Price
        cvt = fromIntegral . parsePips n
        [o', h', l', c'] = map cvt [o, h, l, c]
pbFromText _ ss = error ("Insufficient data for price bar:"++ (show ss))

hasPoint :: String -> Bool
hasPoint = (== 1) . length . filter id . map (== '.')

intDigits  :: String -> String
intDigits = takeWhile (/= '.')

fracDigits :: String -> String
fracDigits s
    | hasPoint s = (tail . dropWhile (/= '.')) s
    | otherwise  = ""

fracDigitsOK :: Int -> String -> Bool
fracDigitsOK n s 
    | hasPoint s = ((<= n) . length . tail . dropWhile (/= '.')) s
    | otherwise  = True

padFracDigits :: Int -> String -> String
padFracDigits n s = take n (fracDigits s ++ repeat '0')

padPips :: Int -> String -> String
padPips n s = intDigits s ++ "." ++ padFracDigits n s

parsePips :: Int -> String -> Int
parsePips n s = readInt (intDigits  s) * (10 ^ n)
              + readInt (padFracDigits n s)

readInt :: String -> Int
readInt s = read s 

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
isBullish pb = close pb > open pb

isBearish = not . isBullish

isPeak :: PriceBar -> PriceBar -> PriceBar -> Bool
isPeak pb1 pb2 pb3 = high pb2 > high pb1 && high pb2 > high pb3

isValley :: PriceBar -> PriceBar -> PriceBar -> Bool
isValley pb1 pb2 pb3 = low pb2 < low pb1 && low pb2 < low pb3

isPeakInRange :: PriceRange -> Extremum -> Bool
isPeakInRange pr (Ext _ Valley) = error "isPeakInRange received a valley"
isPeakInRange pr (Ext pb _) = isPriceBarInRange pr (\x -> high x) pb

isValleyInRange :: PriceRange -> Extremum -> Bool
isValleyInRange pr (Ext _ Peak) = error "isValleyInRange received a peak"
isValleyInRange pr (Ext pb _) = isPriceBarInRange pr (\x -> low x) pb

isExtremumInRange :: PriceRange -> Extremum -> Bool
isExtremumInRange pr x@(Ext pb Peak) = isPeakInRange pr x
isExtremumInRange pr x@(Ext pb Valley) = isValleyInRange pr x
isExtremumInRange pr x@(Ext pb Both) = (isPeakInRange pr x) || (isValleyInRange pr x)

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
                    then Just (Ext t2 Peak)
                    else Just (Ext t2 Valley)
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

