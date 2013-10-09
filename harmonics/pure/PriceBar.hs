module PriceBar where

import CSV
import Graphics.Rendering.OpenGL

type Price = GLfloat

data PriceRange = PriceRange Price Price  deriving (Show, Eq)   -- low, high

data PriceBar a = Pb {
      date     :: String
    , tick     :: Int           -- ^Simple horizontal metric
    , ext      :: Bool          -- ^True if extremum
    , peak     :: Bool          -- ^True if extremum and peak
    , open     :: a
    , high     :: a
    , low      :: a
    , close    :: a
    , volume   :: Integer       -- ^DELETE -- not used in forex pricebars
    , adjClose :: a             -- ^DELETE -- not used in forex pricebars
} deriving (Show, Eq)

parsePriceBars' :: IO String -> IO [[String]]
parsePriceBars' getInput = do
    c <- getInput
    case parseCSV c of
        Left e  -> let s = "Error parsing input: " ++ show e
                   in error s
        Right r -> return r

parsePriceBarsStdin :: IO [[String]]
parsePriceBarsStdin = parsePriceBars' getContents

parsePriceBars :: FilePath -> IO [[String]]
parsePriceBars path = parsePriceBars' (readFile path)

pbsFromFile :: Int -> FilePath -> IO [PriceBar Price]
pbsFromFile n path = do
    ss <- parsePriceBars path
    let pbs = if null ss
                then []
                else map (pbFromText n) (tail ss) -- TODO assumes header row
    return (timestamp pbs)

isFx :: [String] -> Bool
isFx ("<TICKER>":_) = True
isFx _ = False

timestamp :: [PriceBar a] -> [PriceBar a]
timestamp pbs = [fixupTick n pb | (n,pb) <- zip [0..] pbs ]

fixupTick :: Int -> PriceBar a -> PriceBar a
fixupTick n pb = pb { tick = n }

makePb d o h l c = 
    Pb { 
          date = d
        , tick = 0
        , ext  = False
        , peak = False
        , open = o
        , high = h
        , low  = l
        , close = c
        , volume = 0
        , adjClose = 0
    }

pbFromText :: Int -> [String] -> PriceBar Price
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
pbFromText _ ss = error ("Insufficient data for price bar:"++ show ss)

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
readInt = read 

priceBarsToTriplets :: [PriceBar a] -> [(PriceBar a,PriceBar a,PriceBar a)]
priceBarsToTriplets pbs = zip3 pbs (tail pbs) ((tail.tail) pbs)

inRange :: Price -> Price -> Price -> Bool
inRange low high test_val = test_val >= low && test_val <= high

doesPriceBarIntersectRange :: PriceRange -> PriceBar Price -> Bool
doesPriceBarIntersectRange (PriceRange lo hi) pb = (high pb > lo) && (low pb < hi)

isPriceBarInRange :: PriceRange -> (PriceBar a -> Price) -> PriceBar a -> Bool
isPriceBarInRange (PriceRange lo hi) getPrice pb = inRange lo hi (getPrice pb)

highestHigh pbs = maximum (map high pbs)
lowestLow pbs = minimum (map low pbs)

isBullish :: (Ord a, Num a) => PriceBar a -> Bool
isBullish pb = close pb > open pb

isBearish = not . isBullish

