-- |A model of a brokerage account for currency trading.
-- Everything is exposed in this module for testing.
-- To use Account in a program, import module Account
-- which re-exports the public interface.
module Acct
where

import Data.List(delete,partition)
import Data.Ord(Ordering(..))
import PriceBar
import Utilities

-- |A unit of order or trade
data Contract = Con {
      cName  :: String  -- ^The symbol being traded
    , cLong  :: Bool    -- ^The dispostion of the trade
                        -- (True for long or buy, False for short or sell)
    , cQty   :: Int     -- ^The number of contracts
} deriving (Show, Eq)

-- |A commitment to a contract at a given price
data Position = Pos {
      con    :: Contract -- ^Contract details
    , pPrice :: Price    -- ^The price at which the contract was traded
} deriving (Show, Eq)

pName :: Position -> String
pName = cName . con

pLong :: Position -> Bool
pLong = cLong . con

pQty :: Position -> Int
pQty = cQty . con

-- |A model of an order
data Order = 
      MktOrd   { oCon::Contract }
    | StopOrd  { oCon::Contract, strike::Price }
    | LimitOrd { oCon::Contract, strike::Price }
    deriving (Show, Eq)

-- |A representation of a trading account
data Account = Acct {
      acctStdContract  :: Int
    , acctLeverage     :: Int
    , acctReserve      :: Price
    , acctBalance      :: Price
    , acctLastPrice    :: Price
    , acctPositions    :: [Position]
    , acctOrders       :: [Order]
    , acctCanExec      :: (Account -> Contract -> Price -> Bool)
}

instance Show Account where
    show = formatAccount

instance Eq Account where
    (==) = acctAreEqual

acctAreEqual acct acct' = and checks
    where
        check :: Eq a => Account -> Account -> (Account -> a) -> Bool
        check acct acct' f = f acct == f acct'
        checks = [check acct acct' acctStdContract
                , check acct acct' acctLeverage
                , check acct acct' acctReserve
                , check acct acct' acctBalance
                , check acct acct' acctPositions
                , check acct acct' acctOrders]

formatAccount acct = ""
    ++ "\nBalance:      " ++ show (acctBalance acct)
    ++ "\nStd contract: " ++ show (acctStdContract acct)
    ++ "\nLeverage:     " ++ show (acctLeverage acct)
    ++ "\nReserve:      " ++ show (floor (acctReserve acct * 100)) ++ "%"
    ++ "\nLast price:   " ++ show (acctLastPrice acct)
    ++ "\nPositions:    " ++ show posTotal
    ++ "\n"               ++ fmtPs (acctPositions acct) 
    ++ "Orders:\n"        ++ fmtOs (acctOrders acct)
    where 
        indent = map (\s->"  "++s)
        md = marginDeposit acct
        pr = acctLastPrice acct
        posTotal = sum (map (posValue pr md) (acctPositions acct))
        fmtPs = unlines . indent . (map (formatPositionValue pr md))
        fmtOs = unlines . indent . (map formatOrder)

mkPosition :: String -> Bool -> Int -> Price -> Position
mkPosition name t n pr = Pos (Con name t n) pr

mkContract :: String -> Bool -> Int -> Contract
mkContract name t n = Con name t n

isLongPosition :: Position -> Bool
isLongPosition p = pLong p

isShortPosition :: Position -> Bool
isShortPosition p = not (pLong p)

longContracts :: [Position] -> [Position]
longContracts = filter isLongPosition

shortContracts :: [Position] -> [Position]
shortContracts = filter isShortPosition

numContracts :: [Position] -> Int
numContracts = sum . (map pQty)

-- |Initialize an account data structure.
initAccount :: Int      -- ^The size of a standard contract
            -> Int      -- ^The trading leverage offered by the broker
            -> Price    -- ^The reserve balance to be maintained
            -> Price    -- ^The initial balance
            -> Account  -- ^The initialized account
initAccount cs lv r b = Acct {
      acctStdContract = cs
    , acctLeverage = lv
    , acctReserve = r
    , acctBalance = b
    , acctLastPrice = 0
    , acctPositions = []
    , acctOrders = []
    , acctCanExec = acctIsTxnValid
}

updateLastPrice :: Account -> Price -> Account
updateLastPrice acct pr = acct { acctLastPrice = pr }

acctContractSize :: Account -> Int
acctContractSize acct = acctStdContract acct `div` acctLeverage acct

acctNumLongContracts :: Account -> Int
acctNumLongContracts = numContracts . longContracts . acctPositions

acctNumShortContracts :: Account -> Int
acctNumShortContracts = numContracts . shortContracts . acctPositions

acctIsTxnValid :: Account -> Contract -> Price -> Bool
acctIsTxnValid acct c pr = b >= margin md c
    where
        b  = acctBalance acct
        md = marginDeposit acct

-- |If true, indicates the account has at least one long position.
isLong :: Account -> Bool
isLong acct = acctNumLongContracts acct /= 0

-- |If true, indicates the account has at least one short position.
isShort :: Account -> Bool
isShort acct = acctNumShortContracts acct /= 0

posValue :: Price -> Price -> Position -> Price
posValue pr md p = md * fromIntegral (pQty p) * (pr / pPrice p)

posValueChange :: Price -> Price -> Position -> Price
posValueChange pr md p = md * fromIntegral (pQty p) * (pr / pPrice p - 1)

acctValue :: Account -> Price -> Price
acctValue acct pr = b + sum (map (posValue pr md) ps)
    where
        b  = acctBalance   acct
        md = marginDeposit acct
        ps = acctPositions acct

solvent :: Account -> Price -> Bool
solvent acct = (> 0) . acctValue acct

-- |Takes n units out of position p, if possible. Returns the
-- amount taken out, followed by the amount left in.
-- Requires position p to have pQty no less than n.
splitPosition :: Position -> Int -> (Maybe Position, Maybe Position)
splitPosition p n 
    | (pQty p) >= n  =  (mkPos n_out, mkPos n_in)
    | otherwise      =  error msg
    where
        (n_out, n_in) = splitNum (pQty p) n
        mkPos m = if m > 0 then Just (p {con=(con p){cQty=m}}) else Nothing
        msg = "splitPosition: pQty=" ++ show (pQty p) ++ " < n=" ++ show n

opposedToP :: Position -> Contract -> Bool
opposedToP p c = (pName p == cName c) && (pLong p /= cLong c)

opposedToC :: Contract -> Position -> Bool
opposedToC c p = (cName c == pName p) && (cLong c /= pLong p)

opposingPositions :: Position -> [Position] -> [Position]
opposingPositions p = filter ((opposedToP p) . con)

opposingPositions2 :: Position -> (a -> Position) -> [a] -> [a]
opposingPositions2 p f = filter ((opposedToP p) . con . f)

-- |Match existing positions opposed to an order.
-- Return:
--      new positions (i.e. original less matched plus any remainder
--          from splitting a position)
--      positions to exit (i.e. matched )
--      unmatched part of the order
matchOrder :: [Position] 
           -> Order 
           -> ([Position],[Position],Int)
matchOrder ps o = (new_ps, matched_ps, unmatched_n)
    where
        new_ps      = sortStripList (umzops ++ znops)
        matched_ps  = sortStripList mzops
        unmatched_n = n - (sum . map pQty) matched_ps

        --zps    = zipped positions
        --zops   = zipped opposing positions
        --znops  = zipped non-opposing positions
        --mzops  = matched zipped opposing positions
        --umzops = unmatched zipped opposing positions
        zps = zipWithIndex ps
        ( zops, znops) = partition ((opposedToC c) . fst) zps
        (mzops,umzops) = fifoSplit2 n zops zpQty mkZp 

        -- Details of the order to match
        c = oCon o;  n = cQty c

        --zpQty = get amount from zipped position
        --mkZp  = make a zipped position with an updated amount
        zpQty zp = pQty (fst zp)
        mkZp zp n = ((fst zp) {con=(con (fst zp)){cQty=n}}, snd zp)

mkMarketOrder :: String -> Bool -> Int -> Order
mkMarketOrder nm t n = MktOrd (Con nm t n)

mkStopOrder :: String -> Bool -> Int -> Price -> Order
mkStopOrder nm t n s = StopOrd (Con nm t n) s

mkLimitOrder :: String -> Bool -> Int -> Price -> Order
mkLimitOrder nm t n s = LimitOrd (Con nm t n) s

addOrder :: Account -> Order -> Account
addOrder acct o = acct { acctOrders = o : acctOrders acct }

addOrders :: Account -> [Order] -> Account
addOrders acct os = acct { acctOrders = reverse os ++ acctOrders acct }

--TODO return error if order not found?
cancelOrder :: Account -> Order -> Account
cancelOrder acct o = acct { acctOrders = delete o (acctOrders acct) }

cancelOrders :: Account -> [Order] -> Account
cancelOrders acct [] = acct
cancelOrders acct (o:os) = cancelOrders (cancelOrder acct o) os

cancelOpposingOrders :: Account -> Position -> Account
cancelOpposingOrders acct p = cancelOrders acct oos
    where
        oos = filter ((opposedToP p) . oCon) (acctOrders acct)

triggered :: Price -> Order -> Bool
triggered pr (MktOrd _) = True
triggered pr (StopOrd  c s) = if cLong c then pr >= s else pr <= s
triggered pr (LimitOrd c s) = if cLong c then pr <= s else pr >= s

execOrders :: Account
           -> Price         -- current price
           -> Either
               (Account,    -- updated account
               Price,       -- debit/credit to account balance
               [String])    -- transaction log entries
               String       -- error message
execOrders acct pr =
    if null os
        then Left (acct, 0, [])
        else execOrders' acct os pr
    where
        os = filter (triggered pr) (acctOrders acct)

-- |Execute a list of orders within an account at a price.
-- The orders are assumed to have been triggered, so every
-- order will be executed. Failure of any order will cause
-- all to be aborted.
execOrders' :: Account      -- ^The account
           -> [Order]       -- ^The list of orders
           -> Price         -- ^The price at which to execute the orders
           -> Either
               (Account,
               Price,
               [String])
               String       -- ^The updated account, debit/credit to
                            -- the account balance, and transaction
                            -- log entries, or an error message
execOrders' acct (o:[]) pr =
    if ok then Left results else Right msg
    where
        msg = ":ERROR: failed to execute orders "
            ++ (case eo of Right m -> "\n" ++ m; otherwise -> "")
            ++ "\n"

        results = (acct',db',msgs')

        ok = case eo of Left _ -> True; otherwise -> False
        eo = execOrder acct o pr
        (db', ps', msgs') =
            case eo of
                Left (x, y, z) -> (x, y, z)
                otherwise      -> (0, [], [])

        -- Assimilate results into account
        acct' = acct {
                      acctBalance = acctBalance acct + db'
                    , acctPositions = ps'
                    , acctOrders = delete o (acctOrders acct)
                }

execOrders' acct (o:os) pr =
    if ok then Left results else Right msg
    where
        msg = "Error: failed to execute orders "
            ++ (case eo2' of Right m -> "\n" ++ m; otherwise -> "")
            ++ "\n"

        results = (acct''',db''',msgs''')

        ok = case eo2' of Left _ -> True; otherwise -> False

        eo2  = execOrders' acct [o] pr
        eo2' = case eo2 of
                    Left (acct', _, _) -> execOrders' acct' os pr
                    otherwise          -> eo2

        db''' = case eo2 of
                    Left (acct', db', msgs') ->
                        case eo2' of
                            Left (acct'', db'', msgs'') -> db' + db''
                            otherwise -> error "execOrders' 1"
                    otherwise -> error "execOrders' 2"

        msgs''' = case eo2 of
                    Left (_, _, msgs') ->
                        case eo2' of
                            Left (_, _, msgs'') -> msgs' ++ msgs''
                            otherwise -> error "execOrders' 3"
                    otherwise -> error "execOrders' 4"

        acct''' = case eo2' of
                        Left (acct'', _, _) -> acct''
                        otherwise -> error "execOrders' 5"

-- |Execute an order of any type at a price. Opposing positions
-- will be closed before a new position, if any, is entered.
-- The arguments are not validated.
execOrder :: Account        -- ^The account
          -> Order          -- ^The order to execute
          -> Price          -- ^The price at which to execute
          -> Either
              (Price,
              [Position],
              [String])
              String        -- ^The debit/credit to account balance,
                            -- updated account positions, and transaction
                            -- log entries, or an error message
execOrder acct o pr =
    if ok then Left results else Right msg
    where
        -- Determine the overall success of order execution.
        ok = have_no_emo ||
             (case emo of Left _ -> True; otherwise -> False)
        have_no_emo = n' == 0
        
        msg = "Error: failed to execute order "
            ++ formatOrder o
            ++ (case emo of Right m -> "\n" ++ m; otherwise -> "")
            ++ "\n"

        results = (db'',ps'',msgs'')
        (db'',ps'',msgs'') =
            if have_no_emo
                then (db, nops, msgs)
                else
                    case emo of
                        Left (x,y,z) -> (db + x, nops ++ [y], msgs ++ [z])
                        otherwise    -> (db, nops, msgs)

        -- Match opposing orders.
        -- nops = non-opposing positions
        -- mps  = opposing positions
        -- n'   = number of unopposed contracts in order
        (nops,ops,n') = matchOrder ps o
        ps = acctPositions acct

        -- Exit opposing positions.
        -- db   = debit/credit to account balance
        -- gl   = net gain/loss (not used here)
        -- msgs = transaction log entries
        (db,_,msgs) = exitPositions' ops pr md
        md = marginDeposit acct

        -- Create the market order from the residual part of the
        -- original order, if any, and execute it.
        -- emo = Left (
        --          debit/credit to account balance, 
        --          new position, 
        --          transaction log entries)
        --   or
        --       Right error-message
        c = (oCon o) { cQty = n' }
        acct' = acct {
                      acctBalance = acctBalance acct + db
                    , acctPositions = nops
                }
        emo = if n' > 0
                then execMarketOrder' acct' c pr
                else Right "Asking for emo when n' == 0"

-- |Compute the changes to create and execute a market order
-- for a contract. If the order cannot be executed,
-- an error message is returned instead.
execMarketOrder' :: Account     -- ^The account
                 -> Contract    -- ^The contract details
                 -> Price       -- ^The price at which to execute the order
                 -> Either (Price,
                            Position,
                            String)
                            String
                                -- ^Either the debit to the account balance,
                                -- the new position, and
                                -- the transaction log entry,
                                -- or the error message
execMarketOrder' acct c pr =
    if (acctCanExec acct) acct c pr
        then Left (enterPosition' c pr (marginDeposit acct))
        else Right msg
    where
        msg = ":ERROR: insufficient funds to enter position "
            ++ formatContract c
            ++ " at price " ++ show pr

-- |Take a position on a contract. The arguments are not validated.
enterPosition :: Account    -- ^The account to modify
              -> Contract   -- ^The contract details
              -> Price      -- ^The price at which to enter the position
              -> (Account,
                  Position,
                  String)   -- ^The updated account,
                            -- the new position, and
                            -- the transaction log entry
enterPosition acct c pr = (acct', p, msg)
    where
        acct' = acct {
              acctPositions =  p : acctPositions acct
            , acctBalance   = db + acctBalance acct
        }
        (db, p, msg) = enterPosition' c pr md
        md = marginDeposit acct

-- |Compute the changes to an account in order to take a position
-- on a contract. The arguments are not validated.
enterPosition' :: Contract  -- ^The contract details
               -> Price     -- ^The price at which to enter the position
               -> Price     -- ^The standard margin deposit
               -> (Price,
                   Position,
                   String)  -- ^The debit to the account balance,
                            -- the new position, and
                            -- the transaction log entry
enterPosition' c pr md = (db, p, msg)
    where
        db  = (-1) * fromIntegral (cQty c) * md
        p   = mkPosition (cName c) (cLong c) (cQty c) pr
        msg = formatEnterPosition p

-- |Compute the changes to exit a list of positions.
exitPositions' :: [Position]
               -> Price         -- ^Price at which exiting
               -> Price         -- ^Standard margin deposit
               -> (Price,
                   Price,
                   [String])    -- ^The credit to account balance,
                                -- the net gain/loss, and
                                -- the transaction log entries
exitPositions' ps pr md = foldr f (0, 0, []) ps
    where f p (db, gl, msgs) = (db' + db, gl' + gl, msg' : msgs)
            where (db', gl', msg') = exitPosition' p pr md

-- |Exit a position.
exitPosition :: Account     -- ^The account to modify
             -> Position    -- ^The position to exit
             -> Price       -- ^The price at which to exit
             -> (Account,
                 Price,
                 String)    -- ^The updated account,
                            -- the net gain/loss, and
                            -- and the transaction log entry
exitPosition acct p pr = (acct', gl, msg)
    where
        (db, gl, msg) = exitPosition' p pr (marginDeposit acct)
        acct' = acct {
              acctBalance = db + acctBalance acct
            , acctPositions = delete p (acctPositions acct)
        }

-- |Compute the changes to an account in order to exit a position.
exitPosition' :: Position   -- ^The position to exit
              -> Price      -- ^The price at which to exit
              -> Price      -- ^The standard margin deposit
              -> (Price,
                  Price,
                  String)   -- ^The credit to the account balance
                            -- the net gain/loss, and
                            -- the transaction log entry
exitPosition' p pr md = (db, gl, msg)
    where
        db  = posValue pr md p
        gl  = posValueChange pr md p 
        msg = formatExitPosition p pr gl

formatOrder :: Order -> String
formatOrder (MktOrd c)     = formatOrder' "Market" c Nothing
formatOrder (StopOrd c s)  = formatOrder' "Stop"   c (Just s)
formatOrder (LimitOrd c s) = formatOrder' "Limit"  c (Just s)

formatOrder' :: String -> Contract -> Maybe Price -> String
formatOrder' t c ms = msg
    where
        smsg = case ms of
            Just s    -> " strike=" ++ show s
            otherwise -> ""
        msg = t ++ " "
            ++ formatContract c ++ " "
            ++ smsg

formatEnterPosition :: Position -> String
formatEnterPosition p = ":ENTER " ++ formatPosition p

formatExitPosition :: Position -> Price -> Price -> String
formatExitPosition p pr gl = msg
    where
        msg = ":EXIT " ++ formatPosition p ++ " out:" ++ show pr
            ++ " net:" ++ show gl

formatPosition :: Position -> String
formatPosition p = formatContract (con p) ++ " in=" ++ show (pPrice p)

formatPositionValue :: Price -> Price -> Position -> String
formatPositionValue pr md p = 
    formatContract (con p) ++ " in=" ++ show (pPrice p)
                           ++ " value=" ++ show (posValue pr md p)

formatContract :: Contract -> String
formatContract c = cName c ++ " " 
                    ++ formatDisposition c
                    ++ " n=" ++ show (cQty c)

formatDisposition :: Contract -> String
formatDisposition c = if cLong c then "LONG" else "SHORT"

marginDeposit :: Account -> Price
marginDeposit acct = fromIntegral (acctContractSize acct)

margin :: Price -> Contract -> Price
margin md c = md * fromIntegral (cQty c)

usedMargin :: Account -> Price
usedMargin acct = sumMargins ps
    where
        sumMargins :: [Position] -> Price
        sumMargins = sum . map (margin md) . map con
        md = marginDeposit acct
        ps = acctPositions acct

-- |Increase the account balance by the given amount.
depositFunds :: Price -> Account ->  (Account, Price, String)
depositFunds db acct
    | db >= 0    =  (acct', db, smsg)
    | otherwise  =  (acct,   0, emsg)
    where
        b'    = acctBalance acct + db
        acct' = acct { acctBalance = b' }
        smsg  = ":CREDIT " ++ show db ++ " BALANCE=" ++ show b'
        emsg  = ":ERROR: Failed to credit amount " ++ show db

-- |Reduce the account balance by the given amount, if
-- the account balance is large enough.
withdrawFunds :: Account                -- ^Account to debit
              -> Price                  -- ^Amount to withdraw
              -> Maybe Price            -- ^Optional maximum withdrawal
              -> Either (Account, String) String
                                        -- ^Updated account and message
                                        -- or error string
withdrawFunds acct db Nothing =
    withdrawFunds acct db (Just (acctBalance acct))
withdrawFunds acct db (Just available)
    | available > b   =  Right emsg1
    | db < 0          =  Right emsg2
    | db > available  =  Right emsg3
    | otherwise       =  Left (acct', smsg)
    where
        acct' = acct { acctBalance = b' }
        b'    = b - db
        b     = acctBalance acct
        smsg  = ":DEBIT " ++ show db ++ " BALANCE=" ++ show b'
        emsg1 = ":ERROR: withdrawFunds: invalid state: available "
                ++ show available ++ " > balance " ++ show b
        emsg2 = ":ERROR: withdrawFunds: attempting to withdraw negative "
                ++ "amount " ++ show db
        emsg3 = ":ERROR: withdrawFunds: attempting to withdraw more "
                ++ "(" ++ show db ++ ")" ++ " than available "
                ++ "(" ++ show available ++ ")"

-- |Exit all open trades.
liquidateTrades :: Account -> Price -> (Account, Price, [String])
liquidateTrades acct pr = (acct', gl, [msg] ++ msgs)
    where
        msg = ":LIQUIDATING ALL POSITIONS"
        b  = acctBalance acct
        ps = acctPositions acct
        md = marginDeposit acct
        (db,gl,msgs) = exitPositions' ps pr md
        b' = max 0 (b + db)
        acct' = acct { acctPositions = [], acctBalance = b' }

-- |Roll over all open trades at the given price.
rolloverTrades :: Account -> Price -> (Account, Price, [String])
rolloverTrades acct pr = rolloverTrades' acct (acctPositions acct) pr

rolloverTrades' :: Account -> [Position] -> Price -> (Account, Price, [String])
rolloverTrades' acct [] _      = (acct, 0, [])
rolloverTrades' acct (p:ps) pr = (acct'', gl' + gl'', msgs' ++ msgs'')
    where
        (acct' , gl' , msgs' ) = rolloverTrade acct p pr
        (acct'', gl'', msgs'') = rolloverTrades' acct' ps pr

-- |Exit a position at the price, incorporate the gain or loss in the
-- account balance, then re-enter the position at the same price.
rolloverTrade :: Account -> Position -> Price -> (Account, Price, [String])
rolloverTrade acct p pr = (acct'', gl, [msg, msg'])
    where
        b  = acctBalance acct
        ps = acctPositions acct
        md = marginDeposit acct
        (db,gl,msg) = exitPosition' p pr md
        acct' = acct {
              acctPositions = delete p ps
            , acctBalance   = b + db 
            }

        b'  = acctBalance acct'
        ps' = acctPositions acct'
        (acct'',_,msg') = enterPosition acct' (con p) pr


