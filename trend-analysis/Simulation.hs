module Simulation(
      SimulationState
    , initSimulation
    , initDataSource
    , runSimulation
    , runAccountProcess
    ) where

import Control.Monad.State
import Data.Maybe(fromJust)
import PriceBar
import ExtremumMatcher
import Account
import TrendAnalysis

initDataSource :: FilePath -> IO [PriceBar]
initDataSource = pbsFromFile

data SimulationState = SimState {
    -- Generic simulation parameters
      ssChartWidth :: Int
    , ssPriceBars :: [PriceBar]
    , ssLastPriceBar :: Maybe PriceBar
    , ssMessages :: [String]

    -- Account info
    , ssAccount :: Account

    -- Application-specific simulation parameters
    , ssExtremumMatcher :: ExtremumMatcherState
    , ssMarketType :: MarketType
}
type Simulation a = StateT SimulationState IO a

initSimulation :: [PriceBar]        -- ^The datasource of price bars
               -> Int               -- ^The number of price bars to use
               -> Int               -- ^The extremum strength
               -> Price             -- ^The initial account balance
               -> SimulationState   -- ^The initialized simulation state
initSimulation pbs n strength b = 
    SimState {
        -- Generic simulation parameters
          ssChartWidth = n
        , ssPriceBars = pbs'
        , ssLastPriceBar = Nothing
        , ssMessages = []

        -- Account info
        , ssAccount = initAccount 100000 100 0.8 b

        -- Application-specific simulation parameters
        -- Note: Extrema of type "Both" are ignored.
        , ssExtremumMatcher = (initExtremumMatcherState strength False)
        , ssMarketType = Indeterminate
    }
    where pbs' = (reverse . (take n) . reverse) pbs

runSimulation :: SimulationState -> ([String] -> IO ()) -> Bool -> IO ()
runSimulation s report verbose =
    if isDone s
        then do
            let v = calcAccountValue (ssAccount s) (ssLastPriceBar s)
            let msgs = finalReport (show (ssAccount s)) v
            report msgs
            return ()
        else do
            (msgs,s') <- simulate doStep s
            report msgs
            runSimulation s' report verbose

simulate = runStateT

isDone :: SimulationState -> Bool
isDone = null . ssPriceBars

-- |Calculate the net worth of an account including open trades.
calcAccountValue :: Account         -- ^The account
                 -> Maybe PriceBar  -- ^The price, if available, at which
                                    -- to evaluate open trades
                 -> Price           -- ^The net worth
calcAccountValue acct Nothing = acctValue acct 0
calcAccountValue acct (Just pb) = acctValue acct (close pb)

finalReport :: String -> Price -> [String]
finalReport s v = [s, ":Account balance: " ++ show v]

clearMessage :: Simulation ()
clearMessage =
    do
        s <- get
        put s { ssMessages = [] }

sguard :: Bool -> Simulation () -> Simulation ()
sguard p a = if p then a else return ()

-- Application-specific simulation step per price bar.
doStep :: Simulation [String]
doStep = do
    
    clearMessage
    
    -- Get initial state.
    s <- get
    
    -- Not supposed to apply this function without data.
    sguard (isDone s) (error "No data")
    
    -- Extract data.
    let pb  = (head . ssPriceBars) s
    let ems = ssExtremumMatcher s
    let mt  = ssMarketType s
    let pr  = close pb
    let acct = ssAccount s
    
    -- Did we encounter a new extremum?
    -- Did the disposition of the market change?
    let (ex,ems')     = runExtremumMatcher (nextExtremum pb) ems
    let ((his,los),_) = runExtremumMatcher getHighsLows ems'
    let mt'           = case marketType his los of
                            Unchanged -> mt
                            mt' -> mt'
    let msgs = formatPbAndEx pb ex mt'

    s' <- get
    put s' {
          ssPriceBars = (tail . ssPriceBars) s'
        , ssLastPriceBar = Just pb
        , ssExtremumMatcher = ems'
        , ssMarketType = mt'
        , ssMessages = ssMessages s' ++ msgs
        , ssAccount = updateLastPrice acct pr
        }

    checkSolvency  pr
    execAcctOrders pr
    execStrategy pr mt mt' los his
    
    s'' <- get
    let msgs'' = ssMessages s''
    return msgs''

runAccountProcess :: (Account -> (Account, Price , [String])) 
                  -> Simulation Price
runAccountProcess f =
    do
        s <- get
        let acct = ssAccount s
        let (acct', gl, msgs) = f acct
        put s {
              ssMessages = ssMessages s ++ msgs
            , ssAccount = acct'
            }
        return gl

checkSolvency :: Price -> Simulation Price
checkSolvency pr = runAccountProcess (f pr)
    where f pr acct = if solvent acct pr
                            then (acct, 0, [])
                            else liquidateTrades acct pr

execAcctOrders :: Price -> Simulation Price
execAcctOrders pr = runAccountProcess (f pr)
    where f pr acct = case execOrders acct pr of
                            Left (acct', gl, msgs) -> (acct', gl, msgs)
                            Right msg -> (acct, 0, [msg])

execStrategy :: Price
             -> MarketType
             -> MarketType
             -> [Extremum]
             -> [Extremum]
             -> Simulation Price
execStrategy pr mt mt' los his =
    do
        s <- get
        let acct = ssAccount s
        let (exitL, exitS, goL, goS) = execStrategy' acct mt mt' his los
        gl  <- runAccountProcess (exitMarket' pr exitS exitL)
        gl' <- runAccountProcess (enterMarket' pr goS goL 
                                  (latestLow los) (latestHigh his) )
        return (gl + gl')

-- Pure functions

execStrategy' :: Account    -- ^The account
              -> MarketType -- ^The market type at previous price bar
              -> MarketType -- ^The market type at current price bar
              -> [Extremum] -- ^The recent swing highs
              -> [Extremum] -- ^The recent swing lows
              -> (Bool,
                  Bool,
                  Bool,
                  Bool)     -- ^The signal to exit a long position,
                            -- the signal to exit a short position,
                            -- the signal to enter a long position, and
                            -- the signal to enter a short position
execStrategy' acct mt mt' his los = (exitLong, exitShort, goLong', goShort')
    where
        -- Decide what to do next.
        (goLong, goShort) = 
             case calcAction mt mt' of
                 Left a -> a
                 Right amsg -> 
                    let camsg = (amsg ++ 
                                "\nHighs:\n" ++ show (take 2 his) ++ 
                                "\nLows:\n" ++ show (take 2 los) ++ "\n" )
                    in error camsg

        -- If it's a bull market, go long. If it's a bearish market, go short.
        -- If it's neutral, stand aside.
        long = isLong acct
        short = isShort acct
        goLong'  = not long && goLong
        goShort' = not short && goShort
        exitLong  = long && not goLong
        exitShort = short && not goShort

exitMarket' pr short long acct =
    if short
        then let (acct', gl') = exitSellTrade acct pr
                 msg = formatExitShort gl'
             in (acct', gl', [msg])
        else if long
                then let (acct', gl') = exitBuyTrade acct pr
                         msg = formatExitLong gl'
                     in (acct', gl', [msg])
                else (acct, 0, [])

enterMarket' pr short long lo hi acct =
    if short
        then let acct' = enterSellTrade acct pr hi
                 msg = formatEnterShort pr hi
             in (acct', 0, [msg])
        else if long
                then let acct' = enterBuyTrade acct pr lo
                         msg = formatEnterLong pr lo
                     in (acct', 0, [msg])
                else (acct, 0, [])

-- Buy up to n% of account's balance.
enterBuyTrade :: Account -> Price -> Price -> Account
enterBuyTrade acct pr pr2 = enterTrade acct True pr pr2

-- Sell up to n% of account's balance.
enterSellTrade :: Account -> Price -> Price -> Account
enterSellTrade acct pr pr2 = enterTrade acct False pr pr2

enterTrade :: Account -> Bool -> Price -> Price -> Account
enterTrade acct t pr pr2 = acct''
    where
        acct'' = if mso == Nothing
                    then acct'
                    else addOrder acct' (fromJust mso)
        mso    = if mp == Nothing 
                    then Nothing 
                    else Just so
        (acct', mp) = enterMaxMarketOrder acct t pr
        so = mkStopOrder name (not t) (pQty (fromJust mp)) pr2
        name = ""

calcAvailable :: Account -> Price
calcAvailable acct = b * (1 - r)
    where
        b = acctBalance acct
        r = acctReserve acct

calcMaxNumContracts :: Account -> Price -> Int
calcMaxNumContracts acct p = fromIntegral (floor (a / (s * p)))
    where
        a = calcAvailable acct
        s = fromIntegral (acctContractSize acct)

enterMaxMarketOrder :: Account -> Bool -> Price 
                    -> (Account, Maybe Position)
enterMaxMarketOrder acct t pr = 
    if n > 0 
        then (acct', Just p)
        else (acct,  Nothing)
    where
        n  = calcMaxNumContracts acct pr
        c = mkContract name t n
        name = ""
        (acct', p, _) = enterPosition acct c pr

exitSellTrade :: Account -> Price -> (Account, Price)
exitSellTrade acct pr = exitTrade acct getShortPos pr

exitBuyTrade :: Account -> Price -> (Account, Price)
exitBuyTrade acct pr = exitTrade acct getLongPos pr

exitTrade :: Account        -- ^The account to modify
          -> (Account -> Either Position String)
                            -- ^A function to produce the position
          -> Price          -- ^The price at which to exit
          -> (Account,
              Price)        -- ^The updated account, and
                            -- the net gain/loss on the trade
exitTrade acct getPos pr = (acct'', gl)
    where
        p = case getPos acct of
                Left pp -> pp
                Right msg -> error msg
        (acct', gl, _) = exitPosition acct p pr
        acct'' = cancelOpposingOrders acct' p

getShortPos :: Account -> Either Position String
getShortPos acct = pos
    where
        ps = acctPositions acct
        pos = if length ps == 1
                then if isShortPosition (head ps)
                        then Left (head ps)
                        else Right "getShortPos called with long position"
                else Right "getShortPos called without positions"

getLongPos :: Account -> Either Position String
getLongPos acct = pos
    where
        ps = acctPositions acct
        pos = if length ps == 1
                then if isLongPosition (head ps)
                        then Left (head ps)
                        else Right "getLongPos called with short position"
                else Right "getLongPos called without positions"


latestLow :: [Extremum] -> Price
latestLow (x:_) = low (priceBar x)

latestHigh :: [Extremum] -> Price
latestHigh (x:_) = high (priceBar x)

-- Result is ((goLong, goShort)
-- Note that successive peaks or valleys with the exact value
-- are considered Indeterminate.
calcAction :: MarketType -> MarketType -> Either (Bool, Bool) String
calcAction Bullish   Megaphone       = Left (False, False)
calcAction Bullish   Bullish         = Left (True,  False)
calcAction Bullish   Pennant         = Left (False,  False)

calcAction Bearish   Megaphone       = Left (False, False)
calcAction Bearish   Bearish         = Left (False, True)
calcAction Bearish   Pennant         = Left (False, False)

calcAction Megaphone Megaphone       = Left (False, False)
calcAction Megaphone Bullish         = Left (True,  False)
calcAction Megaphone Bearish         = Left (False, True)

calcAction Pennant   Bullish         = Left (True,  False)
calcAction Pennant   Bearish         = Left (False, True)
calcAction Pennant   Pennant         = Left (False, False)

calcAction Indeterminate Indeterminate = Left (False, False)
calcAction Indeterminate Megaphone     = Left (False, False)
calcAction Indeterminate Pennant       = Left (False, False)
calcAction Indeterminate Bullish       = Left (True, False)
calcAction Indeterminate Bearish       = Left (False, True)

calcAction oldmt newmt = Right msg
    where msg = "Invalid args to calcAction: "
              ++ (show oldmt) ++ " " ++ (show newmt)

formatPriceBar (PriceBar d o h l c _ _) =
        "o:" ++ (show o) 
    ++ " h:" ++ (show h)
    ++ " l:" ++ (show l)
    ++ " c:" ++ (show c)

formatExitShort gl = ":EXIT SHORT @ " ++ show gl
formatExitLong  gl = ":EXIT LONG @ " ++ show gl
formatEnterShort pr hi =
    ":ENTER SHORT @ " ++ show pr ++ " STOP LONG @ " ++ show hi
formatEnterLong  pr lo =
    ":ENTER LONG @ " ++ show pr ++ " STOP SHORT @ " ++ show lo

formatPbAndEx pb ex mt  = exmsg ++ pbmsg
    where
        exmsg = case ex of
                    Just x -> [show (concavity x) ++ " " ++ show mt]
                    otherwise -> []
        pbmsg = [formatPriceBar pb]

