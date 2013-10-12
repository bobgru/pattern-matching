module TrendAnalysis (
    marketType,
    calcTriggers,
    updateTriggers,
    checkTriggerTripped,
    MarketType(..),
    TriggerType(..)
    ) where

import PriceBar

data MarketType = Bullish 
                | Bearish 
                | Megaphone 
                | Pennant 
                | Indeterminate
                | Unchanged
    deriving (Show, Eq)

data TriggerType = BuyTrigger Price | SellTrigger Price
data TriggerOp = AddBuyTrigger Price 
               | AddSellTrigger Price
               | SubBuyTrigger 
               | SubSellTrigger

makeBuyTrigger :: [Extremum] -> [Extremum] -> TriggerType
makeBuyTrigger ((Extremum pb Peak):_) _ = BuyTrigger (high pb)
makeBuyTrigger _ _ = error "makeBuyTrigger called without last peak"

makeSellTrigger :: [Extremum] -> [Extremum] -> TriggerType
makeSellTrigger _ ((Extremum pb Valley):_) = SellTrigger (low pb)
makeSellTrigger _ _ = error "makeSellTrigger called without last valley"

calcTriggers :: [Extremum] -> [Extremum] -> [Maybe TriggerOp]
calcTriggers [] _ = []
calcTriggers _ [] = []
calcTriggers his los =
    let mbto = checkBuyTrigger his los
        msto = checkSellTrigger his los
--    in map (\(Just t)->t) (filter (/= Nothing) [mbto,msto])
--    in filter (/= Nothing) [mbto,msto]
    in [mbto,msto]

checkBuyTrigger his los =
    if length los < 2
        then Nothing
        else if exLo (last los) <= exLo (last (init los))
            then Just SubBuyTrigger
            else Just (AddBuyTrigger (exHi (last his)))

checkSellTrigger his los =
    if length his < 2
        then Nothing
        else if exHi (last his) <= exHi (last (init his))
            then Just SubSellTrigger
            else Just (AddSellTrigger (exLo (last los)))

updateTriggers :: (Maybe TriggerType, Maybe TriggerType)
               -> [Maybe TriggerOp]
               -> (Maybe TriggerType, Maybe TriggerType)
updateTriggers = foldr mergeTrigger

mergeTrigger :: Maybe TriggerOp
             -> (Maybe TriggerType, Maybe TriggerType)
             -> (Maybe TriggerType, Maybe TriggerType)
mergeTrigger Nothing x = x
mergeTrigger (Just (AddBuyTrigger p)) (Nothing, mst) =
    (Just (BuyTrigger p), mst)
mergeTrigger (Just (AddSellTrigger p)) (mbt, Nothing) =
    (mbt, Just (SellTrigger p))
mergeTrigger (Just (AddBuyTrigger p2)) (Just (BuyTrigger p1), mst) =
    (Just (BuyTrigger (min p1 p2)), mst)
mergeTrigger (Just (AddSellTrigger p2)) (mbt, Just (SellTrigger p1)) =
    (mbt, Just (SellTrigger (max p1 p2)))
mergeTrigger (Just SubBuyTrigger)  (Nothing, mst) = (Nothing, mst)
mergeTrigger (Just SubSellTrigger) (mbt, Nothing) = (mbt, Nothing)
mergeTrigger (Just SubBuyTrigger)  (Just (BuyTrigger _), mst)  = (Nothing, mst)
mergeTrigger (Just SubSellTrigger) (mbt, Just (SellTrigger _)) = (mbt, Nothing)

checkTriggerTripped :: Maybe TriggerType -> PriceBar -> Bool
checkTriggerTripped Nothing _ = False
checkTriggerTripped (Just (BuyTrigger p)) pb = close pb > p
checkTriggerTripped (Just (SellTrigger p)) pb = close pb < p

marketType :: [Extremum] -> [Extremum] -> MarketType
marketType his los
    | length his < 2 || length los < 2 = Indeterminate
    | bullish his los   = Bullish
    | bearish his los   = Bearish
    | megaphone his los = Megaphone
    | pennant his los   = Pennant
    | otherwise         = Unchanged

compareHighsLows :: (Price -> Price -> Bool) 
                 -> (Price -> Price -> Bool) 
                 -> [Extremum] 
                 -> [Extremum] 
                 -> Bool
compareHighsLows hiOp loOp his los =
    (high . priceBar . head) his `hiOp` (high . priceBar . last) his &&
    (low  . priceBar . head) los `loOp` (low  . priceBar . last) los

bullish   = compareHighsLows (>) (>)
bearish   = compareHighsLows (<) (<)
megaphone = compareHighsLows (>) (<)
pennant   = compareHighsLows (<) (>)

