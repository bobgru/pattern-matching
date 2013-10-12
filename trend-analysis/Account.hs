-- |A model of a brokerage account for currency trading.
module Account (
      Account(acctBalance,acctReserve,acctPositions)
    , initAccount
    , acctContractSize
    , acctValue
    , solvent
    , isLong
    , isShort
    , Contract
    , mkContract
    , Position
    , pQty
    , isLongPosition
    , isShortPosition
    , enterPosition
    , exitPosition
    , cancelOpposingOrders
    , execOrders
    , addOrder
    , mkStopOrder
    , liquidateTrades
    , updateLastPrice
    , depositFunds
    )
where

import Acct
