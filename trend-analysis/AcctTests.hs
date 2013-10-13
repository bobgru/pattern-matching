module Main where
import Acct
import PriceBar
import TestFramework
import Data.Maybe(fromJust)
import System.Exit (exitFailure, exitSuccess)

main = do
    let (details, summary) = runTests pureTests
    putStrLn details
    (ioDetails, ioSummary) <- runTestsIO ioTests
    putStrLn ioDetails

    if not (summary && ioSummary) then exitFailure else exitSuccess

pureTests = [
    ("acctValue_noOrders_noPositions",  acctValue_noOrders_noPositions)
  , ("acctValue_noOrders_posGain",      acctValue_noOrders_posGain)
  , ("acctValue_noOrders_posLoss",      acctValue_noOrders_posLoss)
  , ("solvent_yes",                     solvent_yes)
  , ("solvent_no",                      solvent_no)
  , ("depositFunds_success",            depositFunds_success)
  , ("depositFunds_error",              depositFunds_error)
  , ("withdrawFunds_success",           withdrawFunds_success)
  , ("withdrawFunds_success_available", withdrawFunds_success_available)
  , ("withdrawFunds_too_much",          withdrawFunds_too_much)
  , ("withdrawFunds_negative",          withdrawFunds_negative)
  , ("withdrawFunds_bad_state",         withdrawFunds_bad_state)
  , ("acctAreEqual_yes",                acctAreEqual_yes)
  , ("acctAreEqual_no",                 acctAreEqual_no)
  , ("enterPosition_good",              enterPosition_good)
  , ("enterPosition'_good",             enterPosition'_good)
  , ("execMarketOrder'_good",           execMarketOrder'_good)
  , ("execMarketOrder'_bad",            execMarketOrder'_bad)
  , ("numContracts_good",               numContracts_good)
  , ("exitPosition'_no_change",         exitPosition'_no_change)
  , ("exitPosition'_gain",              exitPosition'_gain)
  , ("exitPosition'_loss",              exitPosition'_loss)
  , ("exitPosition_no_change",          exitPosition_no_change)
  , ("exitPosition_gain",               exitPosition_gain)
  , ("exitPosition_loss",               exitPosition_loss)
  , ("exitPositions'_gain",             exitPositions'_gain)
  , ("addOrder_one",                    addOrder_one)
  , ("addOrders_two",                   addOrders_two)
  , ("cancelOrder_one",                 cancelOrder_one)
  , ("cancelOrders_two",                cancelOrders_two)
  , ("cancelOpposingOrders_two",        cancelOpposingOrders_two)
  , ("execOrder_stop",                  execOrder_stop)
  , ("execOrder_limit",                 execOrder_limit)
  , ("execOrder_market",                execOrder_market)
  , ("execOrder_opposing",              execOrder_opposing)
  , ("execOrder_opposing_plus",         execOrder_opposing_plus)
  , ("execOrder_opposing_plusplus",     execOrder_opposing_plusplus)
  , ("execOrders'_one",                 execOrders'_one)
  , ("execOrders'_two",                 execOrders'_two)
  , ("triggered_market",                triggered_market)
  , ("triggered_stop_long",             triggered_stop_long)
  , ("triggered_stop_short",            triggered_stop_short)
  , ("triggered_limit_long",            triggered_limit_long)
  , ("triggered_limit_short",           triggered_limit_short)
  , ("execOrders_none",                 execOrders_none)
  , ("execOrders_one",                  execOrders_one)
  , ("rolloverTrade_one",               rolloverTrade_one)
  , ("rolloverTrades_two",              rolloverTrades_two)
  , ("liquidateTrades_good",            liquidateTrades_good)
  , ("usedMargin_two",                  usedMargin_two)
  ]

{-
    acctAreEqual w/ positions
    acctIsTxnValid w/ reserve
    execOrders w/ errors
-}

ioTests = []

mkPb o h l c = PriceBar "x" o h l c 0 c

setupAcct = (acct, c, l, r, b, md)
    where
        acct = initAccount c l r b
        c = 100000
        l = 100
        r = 0.8
        b = 100000
        md = fromIntegral (c `div` l)

setupPos n pr0 = (p, n, pr0)
    where
        p = mkPosition "x" True n pr0


acctValue_noOrders_noPositions =
    assertAreEqual 100000 (acctValue acct pr)
    where
        (acct, c, l, r, b, md) = setupAcct
        pr = 110

acctValue_noOrders_posGain =
    assertAreEqual expected (acctValue acct' pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (p, n, pr0) = setupPos 1 100
        acct' = acct { acctPositions = [p] }
        pr1 = 110
        v = (md * fromIntegral n) * pr1 / pr0
        expected = b + v

acctValue_noOrders_posLoss =
    assertAreEqual expected (acctValue acct' pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (p, n, pr0) = setupPos 1 120
        acct' = acct { acctPositions = [p] }
        pr1 = 110
        v = (md * fromIntegral n) * pr1 / pr0
        expected = b + v

solvent_yes =
    assertAreEqual True (solvent acct 0)
    where
        (acct, c, l, r, b, md) = setupAcct

solvent_no =
    assertAreEqual False (solvent acct' 0)
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = acct { acctBalance = 0 }

depositFunds_success =
    assertAreEqual expected (depositFunds db acct)
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = acct { acctBalance = b' }
        b' = b + db
        db = 1234
        smsg  = ":CREDIT " ++ show db ++ " BALANCE=" ++ show b'
        expected = (acct', db, smsg)

depositFunds_error =
    assertAreEqual expected (depositFunds db acct)
    where
        (acct, c, l, r, b, md) = setupAcct
        db = (-1234)
        emsg  = ":ERROR: Failed to credit amount " ++ show db
        expected = (acct, 0, emsg)

withdrawFunds_success =
    assertAreEqual expected (withdrawFunds acct db Nothing)
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = acct { acctBalance = 0 }
        db = acctBalance acct
        smsg  = ":DEBIT " ++ show db ++ " BALANCE=" ++ show (0::Price)
        expected = Left (acct', smsg)

withdrawFunds_success_available =
    assertAreEqual expected (withdrawFunds acct db (Just available))
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = acct { acctBalance = b' }
        b' = b - db
        db = acctBalance acct / 2
        available = db + 1
        smsg  = ":DEBIT " ++ show db ++ " BALANCE=" ++ show b'
        expected = Left (acct', smsg)

withdrawFunds_too_much =
    assertAreEqual expected (withdrawFunds acct db Nothing)
    where
        (acct, c, l, r, b, md) = setupAcct
        db = b + 1
        emsg3 = ":ERROR: withdrawFunds: attempting to withdraw more "
                ++ "(" ++ show db ++ ")" ++ " than available "
                ++ "(" ++ show b ++ ")"
        expected = Right emsg3

withdrawFunds_negative =
    assertAreEqual expected (withdrawFunds acct db Nothing)
    where
        (acct, c, l, r, b, md) = setupAcct
        db = (-1)
        emsg2 = ":ERROR: withdrawFunds: attempting to withdraw negative "
                ++ "amount " ++ show db
        expected = Right emsg2

withdrawFunds_bad_state =
    assertAreEqual expected (withdrawFunds acct db (Just available))
    where
        (acct, c, l, r, b, md) = setupAcct
        db = (-1)
        available = b + 1
        emsg1 = ":ERROR: withdrawFunds: invalid state: available "
                ++ show available ++ " > balance " ++ show b
        expected = Right emsg1

acctAreEqual_yes =
    assertAreEqual True (acctAreEqual acct acct')
    where
        acct  = initAccount 1234 5678 0.001 3e5
        acct' = initAccount 1234 5678 0.001 3e5

acctAreEqual_no =
    assertAreEqual False actual
    where
        actual = case1 || case2 || case3 || case4
        case1 = acctAreEqual (initAccount 1234 5678 0.001 3e5)
                             (initAccount 1235 5678 0.001 3e5)
        case2 = acctAreEqual (initAccount 1234 5678 0.001 3e5)
                             (initAccount 1234 5679 0.001 3e5)
        case3 = acctAreEqual (initAccount 1234 5678 0.001 3e5)
                             (initAccount 1234 5679 0.002 3e5)
        case4 = acctAreEqual (initAccount 1234 5678 0.001 3e5)
                             (initAccount 1234 5679 0.002 4e5)

enterPosition_good =
    assertAreEqual expected (enterPosition acct cn pr)
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = acct {
              acctPositions =  p : acctPositions acct
            , acctBalance   = db + acctBalance acct
        }
        cn = mkContract "FOO" True 5
        p = mkPosition "FOO" True 5 pr
        pr = 100
        db = (-1) * margin md cn
        msg = formatEnterPosition p
        expected = (acct', p, msg)

enterPosition'_good =
    assertAreEqual expected (enterPosition' cn pr md)
    where
        (acct, c, l, r, b, md) = setupAcct
        cn = mkContract "FOO" True 5
        p = mkPosition "FOO" True 5 pr
        pr = 100
        db = (-1) * margin md cn
        msg = formatEnterPosition p
        expected = (db, p, msg)

execMarketOrder'_good =
    assertAreEqual expected (execMarketOrder' acct cn pr)
    where
        (acct, c, l, r, b, md) = setupAcct
        cn = mkContract "FOO" True 5
        p = mkPosition "FOO" True 5 pr
        pr = 100
        db = (-1) * margin md cn
        msg = formatEnterPosition p
        expected = Left (db, p, msg)

execMarketOrder'_bad =
    assertAreEqual expected (execMarketOrder' acct cn pr)
    where
        (acct, c, l, r, b, md) = setupAcct
        cn = mkContract "FOO" True 50000
        pr = 100
        msg = ":ERROR: insufficient funds to enter position "
            ++ formatContract cn
            ++ " at price " ++ show pr
        expected = Right msg

numContracts_good =
    assertAreEqual expected (numContracts (acctPositions acct''))
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct', _,_) = enterPosition acct  cn1 pr
        (acct'',_,_) = enterPosition acct' cn2 pr
        cn1 = mkContract "FOO" True 4
        cn2 = mkContract "FOO" True 6
        pr = 100
        expected = 10

exitPosition'_no_change =
    assertAreEqual expected (exitPosition' p pr1 md)
    where
        (acct, c, l, r, b, md) = setupAcct
        p = mkPosition "FOO" True 5 pr0
        pr0 = 100
        pr1 = 100
        db = 5000
        gl = 0
        msg = formatExitPosition p pr1 gl
        expected = (db, gl, msg)

exitPosition'_gain =
    assertAreEqual expected (exitPosition' p pr1 md)
    where
        (acct, c, l, r, b, md) = setupAcct
        p = mkPosition "FOO" True 5 pr0
        pr0 = 100
        pr1 = 1000
        db = 50000
        gl = 45000
        msg = formatExitPosition p pr1 gl
        expected = (db, gl, msg)

exitPosition'_loss =
    assertAreEqual expected (exitPosition' p pr1 md)
    where
        (acct, c, l, r, b, md) = setupAcct
        p = mkPosition "FOO" True 5 pr0
        pr0 = 100
        pr1 = 10
        db = 500
        gl = -4500
        msg = formatExitPosition p pr1 gl
        expected = (db, gl, msg)

exitPosition_no_change =
    assertAreEqual expected (exitPosition acct' p pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct', p, _) = enterPosition acct cn pr0
        acct'' = acct' { acctBalance = b, acctPositions = [] }
        cn = mkContract "FOO" True 5
        pr0 = 100
        pr1 = 100
        db = 5000
        gl = 0
        msg = formatExitPosition p pr1 gl
        expected = (acct'', gl, msg)

exitPosition_gain =
    assertAreEqual expected (exitPosition acct'' p pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct',  p,  _) = enterPosition acct  cn  pr0
        (acct'', p2, _) = enterPosition acct' cn2 pr1
        acct''' = acct'' { acctBalance = b' + db, acctPositions = [p2] }
        b'  = acctBalance acct''
        cn  = mkContract "FOO" True 5
        cn2 = mkContract "BAR" True 10
        pr0 = 100
        pr1 = 1000
        db  = 50000
        gl  = 45000
        msg = formatExitPosition p pr1 gl
        expected = (acct''', gl, msg)

exitPosition_loss =
    assertAreEqual expected (exitPosition acct' p pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct', p, _) = enterPosition acct cn pr0
        acct'' = acct' { acctBalance = b' + db, acctPositions = [] }
        b'  = acctBalance acct'
        cn  = mkContract "FOO" True 5
        pr0 = 100
        pr1 = 10
        db  = 500
        gl  = -4500
        msg = formatExitPosition p pr1 gl
        expected = (acct'', gl, msg)

exitPositions'_gain =
    assertAreEqual expected (exitPositions' ps pr1 md)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct',  p1, _) = enterPosition acct  cn1 pr0
        (acct'', p2, _) = enterPosition acct' cn2 pr1
        cn1 = mkContract "FOO" True 5
        cn2 = mkContract "BAR" True 10
        pr0 = 100
        pr1 = 1000
        db  = 60000
        gl  = 45000
        ps  = [p1, p2]
        msgs = [formatExitPosition p1 pr1 gl, formatExitPosition p2 pr1 0]
        expected = (db, gl, msgs)

addOrder_one =
    assertAreEqual expected (addOrder acct o)
    where
        (acct, c, l, r, b, md) = setupAcct
        o = mkStopOrder "FOO" True 5 100
        expected = acct { acctOrders = [o] }

addOrders_two =
    assertAreEqual expected (addOrders acct [o1, o2])
    where
        (acct, c, l, r, b, md) = setupAcct
        o1 = mkStopOrder "FOO" True 5 100
        o2 = mkStopOrder "FOO" True 10 101
        expected = acct { acctOrders = [o2, o1] }

cancelOrder_one =
    assertAreEqual expected (cancelOrder acct' o1)
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = addOrders acct [o1, o2]
        o1 = mkStopOrder "FOO" True 5 100
        o2 = mkStopOrder "FOO" True 10 101
        expected = acct' { acctOrders = [o2] }

cancelOrders_two =
    assertAreEqual expected (cancelOrders acct' [o1,o3])
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = addOrders acct [o1, o2, o3, o4]
        o1 = mkStopOrder "FOO" True 5 100
        o2 = mkStopOrder "FOO" True 10 101
        o3 = mkStopOrder "BAR" False 15 101
        o4 = mkStopOrder "BAR" False 20 100
        expected = acct' { acctOrders = [o4,o2] }

cancelOpposingOrders_two =
    assertAreEqual expected (cancelOpposingOrders acct' p)
    where
        (acct, c, l, r, b, md) = setupAcct
        acct' = addOrders acct [o1, o2, o3, o4]
        o1 = mkStopOrder "FOO" True 5 100
        o2 = mkStopOrder "FOO" True 10 101
        o3 = mkStopOrder "BAR" False 15 101
        o4 = mkStopOrder "BAR" False 20 100
        p = mkPosition "BAR" True 1 1
        expected = acct' { acctOrders = [o2,o1] }

execOrder_helper o pr =
    assertAreEqual expected (execOrder acct o pr)
    where
        (acct, c, l, r, b, md) = setupAcct
        p    = mkPosition name long qty pr
        name = (cName . oCon) o
        long = (cLong . oCon) o
        qty  = (cQty  . oCon) o
        msgs = [formatEnterPosition p]
        db   = (-1) * posValue pr md p
        expected = Left (db, [p], msgs)

execOrder_stop   = execOrder_helper (mkStopOrder   "FOO" True  5 100) 101
execOrder_limit  = execOrder_helper (mkLimitOrder  "FOO" False 5 100) 101
execOrder_market = execOrder_helper (mkMarketOrder "FOO" False 5)     101

-- The order exactly opposes the one position in the account.
execOrder_opposing =
    assertAreEqual expected (execOrder acct' o pr1)
    where
        (acct, c, l, r, b, md) = setupAcct

        -- The original order.
        o = mkMarketOrder "FOO" False 5

        -- Enter a position that the order completely opposes.
        (acct',  p, _) = enterPosition acct cn pr0
        cn   = mkContract name long qty
        name = (cName . oCon) o
        long = (not . cLong . oCon) o
        qty  = (cQty  . oCon) o

        pr0 = 100
        pr1 = 110

        msgs = [formatExitPosition p pr1 (posValueChange pr1 md p)]
        db   = posValue pr1 md p
        expected = Left (db, [], msgs)

-- The order opposes part of the one position in the account, so the
-- remaining unopposed part of the order is executed as a market order.
execOrder_opposing_plus =
    assertAreEqual expected (execOrder acct' o pr1)
    where
        (acct, c, l, r, b, md) = setupAcct

        -- The original order.
        o    = mkMarketOrder "FOO" False 5

        -- Enter a position that the order partially opposes.
        (acct',  p1, _) = enterPosition acct cn pr0
        cn   = mkContract name long qty
        name = (cName . oCon) o
        long = (not . cLong . oCon) o
        qty  = (cQty  . oCon) o - 1
                
        -- The position entered into by the unopposed part of the order.
        p2   = mkPosition name (not long) 1 pr1

        pr0  = 100
        pr1  = 110
        
        msgs = [formatExitPosition p1 pr1 (posValueChange pr1 md p1),
                formatEnterPosition p2]
        db   = (posValue pr1 md p1) - (posValue pr1 md p2) 
        expected = Left (db, [p2], msgs)

-- The order opposes multiple positions in the account with a
-- remaining unopposed part.
execOrder_opposing_plusplus =
    assertAreEqual expected (execOrder acct'' o pr1)
    where
        (acct, c, l, r, b, md) = setupAcct

        -- The original order.
        o    = mkMarketOrder name long qty
        name = "FOO"
        long = False
        qty  = 5

        -- Enter positions that the order partially opposes.
        (acct',   p1, _) = enterPosition acct  cn1 pr0
        cn1   = mkContract name (not long) 1
        (acct'',  p2, _) = enterPosition acct' cn2 pr0
        cn2   = mkContract name (not long) 3

        -- The position entered into by the unopposed part of the order.
        p3    = mkPosition name long 1 pr1

        pr0   = 100
        pr1   = 110

        msgs  = [formatExitPosition p2 pr1 (posValueChange pr1 md p2),
                 formatExitPosition p1 pr1 (posValueChange pr1 md p1),
                 formatEnterPosition p3]
        db    = (posValue pr1 md p1) 
              + (posValue pr1 md p2) 
              - (posValue pr1 md p3) 
        expected = Left (db, [p3], msgs)

execOrders'_one =
    assertAreEqual expected (execOrders' acct [o] pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        o = mkStopOrder   "FOO" True  5 pr0
        p    = mkPosition name long qty pr1
        name = (cName . oCon) o
        long = (cLong . oCon) o
        qty  = (cQty  . oCon) o

        pr0   = 100
        pr1   = 101

        msgs = [formatEnterPosition p]
        db   = (-1) * posValue pr1 md p
        acct' = acct {
              acctBalance = b + db
            , acctOrders = []
            , acctPositions = [p] }
        expected = Left (acct', db, msgs)

-- Since execOrders' executes all orders, this will result in
-- two positions. Note however that order o2 is not triggered
-- by the price and should not be executed, i.e. it should never
-- have been supplied as an argument to execOrders'.
execOrders'_two =
    assertAreEqual expected (execOrders' acct [o1,o2] pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        o1 = mkStopOrder   "FOO" True  5 pr0
        o2 = mkStopOrder   "BAR" False 5 pr0

        p1    = mkPosition (cName c1) (cLong c1) (cQty c1) pr1
        c1   = oCon o1
        
        p2    = mkPosition (cName c2) (cLong c2) (cQty c2) pr1
        c2   = oCon o2

        pr0   = 100
        pr1   = 101

        msgs = [ formatEnterPosition p1
               , formatEnterPosition p2 ]
        db   = (-1) * posValue pr1 md p1
             + (-1) * posValue pr1 md p2
        acct' = acct {
              acctBalance = b + db
            , acctOrders = []
            , acctPositions = [p1, p2] }
        expected = Left (acct', db, msgs)
        
triggered_market = assertAreEqual True actual
    where actual = triggered 100 (mkMarketOrder "FOO" True 1)

triggered_stop_long = assertAreEqual True actual
    where actual = triggered 101 (mkStopOrder "FOO" True  1 100) &&
                   not (triggered 99 (mkStopOrder "FOO" True  1 100))
    
triggered_stop_short = assertAreEqual True actual
    where actual = triggered 99 (mkStopOrder  "FOO" False 1 100) &&
                   not (triggered 101 (mkStopOrder  "FOO" False 1 100))

triggered_limit_long = assertAreEqual True actual
    where actual = triggered  99 (mkLimitOrder "FOO" True  1 100) &&
                   not (triggered 101 (mkLimitOrder "FOO" True  1 100))
                        
triggered_limit_short = assertAreEqual True actual
    where actual = triggered 101 (mkLimitOrder "FOO" False 1 100) &&
                   not (triggered 99 (mkLimitOrder "FOO" False 1 100))

-- Since execOrders executes only triggered orders, this test 
-- will result in no change to the account.
execOrders_none =
   assertAreEqual expected (execOrders acct' pr2)
   where
       (acct, c, l, r, b, md) = setupAcct
       acct' = addOrders acct [o1, o2]
       o1 = mkStopOrder   "FOO" True  5 pr0
       o2 = mkStopOrder   "BAR" False 5 pr1

       pr0   = 100      -- stop long will trigger above
       pr1   =  90      -- stop short will trigger below
       pr2   =  95      -- middle price will trigger neither

       msgs = []
       db   = 0
       expected = Left (acct', db, msgs)

-- This test will result in one order being executed while the
-- other is not.
execOrders_one =
   assertAreEqual expected (execOrders acct' pr2)
   where
       (acct, c, l, r, b, md) = setupAcct
       acct' = addOrders acct [o1, o2]
       o1 = mkStopOrder   "FOO" True  5 pr0
       o2 = mkStopOrder   "BAR" False 5 pr1

       pr0   = 100      -- stop long will trigger above
       pr1   =  90      -- stop short will trigger below
       pr2   = 101      -- price will trigger o1

       p1    = mkPosition (cName c1) (cLong c1) (cQty c1) pr2
       c1   = oCon o1

       msgs = [ formatEnterPosition p1 ]
       db   = (-1) * posValue pr2 md p1
       acct'' = acct' {
             acctBalance = b + db
           , acctOrders = [o2]
           , acctPositions = [p1] }
       expected = Left (acct'', db, msgs)

rolloverTrade_one =
    assertAreEqual expected (rolloverTrade acct' p1 pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct',  p1, _) = enterPosition acct  cn1 pr0
        p2 = mkPosition (pName p1) (pLong p1) (pQty p1) pr1 
        acct'' = acct' { acctBalance = b' + db, acctPositions = [p2] }
        b'   = acctBalance acct'
        cn1  = mkContract "FOO" True 5
        pr0  = 100
        pr1  = 1000
        db   = 50000 - posValue pr1 md p2
        gl   = 45000
        msgs = [formatExitPosition p1 pr1 gl, formatEnterPosition p2]
        expected = (acct'', gl, msgs)

rolloverTrades_two =
    assertAreEqual expected (rolloverTrades acct'' pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct',  p1, _) = enterPosition acct  cn1 pr0
        (acct'', p2, _) = enterPosition acct' cn2 pr0
        p3   = mkPosition (pName p1) (pLong p1) (pQty p1) pr1
        p4   = mkPosition (pName p2) (pLong p2) (pQty p2) pr1
        acct''' = acct'' { acctBalance = b' + db, acctPositions = [p3,p4] }
        b'   = acctBalance acct''
        cn1  = mkContract "FOO" True 5
        cn2  = mkContract "BAR" True 10
        pr0  = 100
        pr1  = 1000
        db   = 50000 + 100000 - posValue pr1 md p3 - posValue pr1 md p4
        gl1  = 45000
        gl2  = 90000
        msgs = [
              formatExitPosition p2 pr1 gl2
            , formatEnterPosition p4
            , formatExitPosition p1 pr1 gl1
            , formatEnterPosition p3
            ]
        expected = (acct''', gl1 + gl2, msgs)

liquidateTrades_good =
    assertAreEqual expected (liquidateTrades acct'' pr1)
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct',  p1, _) = enterPosition acct  cn1 pr0
        (acct'', p2, _) = enterPosition acct' cn2 pr0
        acct''' = acct'' { acctBalance = b' + db, acctPositions = [] }
        b'   = acctBalance acct''
        cn1  = mkContract "FOO" True 5
        cn2  = mkContract "BAR" True 10
        pr0  = 100
        pr1  = 1000
        db   = 50000 + 100000
        gl1  = 45000
        gl2  = 90000
        msgs = [
              ":LIQUIDATING ALL POSITIONS"
            , formatExitPosition p2 pr1 gl2
            , formatExitPosition p1 pr1 gl1
            ]
        expected = (acct''', gl1 + gl2, msgs)

usedMargin_two =
    assertAreEqual expected (usedMargin acct'')
    where
        (acct, c, l, r, b, md) = setupAcct
        (acct',  p1, _) = enterPosition acct  cn1 pr0
        (acct'', p2, _) = enterPosition acct' cn2 pr0
        cn1  = mkContract "FOO" True 5
        cn2  = mkContract "BAR" True 10
        pr0  = 100
        expected = 15 * md
