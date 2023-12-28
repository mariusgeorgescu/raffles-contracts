{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Contracts.Raffle where

import Jambhala.Plutus hiding (Address)
import Jambhala.Utils
import Plutus.V1.Ledger.Interval (after, before)
import Plutus.V1.Ledger.Value (geq)
import Plutus.V2.Ledger.Api
import PlutusTx.Builtins (blake2b_256)
import PlutusTx.Eq qualified as PlutusTx

-- 1. Declare Types
--  Define script parameters type

data RaffleValidatorParams = RaffleValidatorParams
  {raffleStateTokenPolicy :: MintingPolicyHash}
  deriving (Generic, FromJSON, ToJSON)

--  Generate Lift instance
makeLift ''RaffleValidatorParams

data RaffleParams = RaffleParams
  { closingWindow :: Integer -- Miliseconds
  , minRevealingWindow :: Integer -- Miliseconds
  , donationPKH :: PubKeyHash
  , maxNoOfTickets :: Integer
  }
  deriving (Generic, FromJSON, ToJSON)

instance PlutusTx.Eq RaffleParams where
  (==) :: RaffleParams -> RaffleParams -> Bool
  (==) r1 r2 =
    pand
      [ closingWindow r1 #== closingWindow r2
      , minRevealingWindow r1 #== minRevealingWindow r2
      , donationPKH r1 #== donationPKH r2
      , maxNoOfTickets r1 #== maxNoOfTickets r2
      ]

--  Generate Lift instance
makeLift ''RaffleParams

--  Define any required custom data types

data RaffleTicket = RaffleTicket
  { ticketOwner :: PubKeyHash
  , ticketNumber :: Integer
  , ticketSecretHash :: BuiltinByteString
  , ticketSecret :: Maybe BuiltinByteString
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleDatum = RaffleDatum
  { raffleStateTokenAssetClass :: AssetClass
  , raffleOrganizer :: PubKeyHash
  , rafflePrizeValue :: Value
  , raffleTicketPrice :: Integer
  , raffleMinNoOfTickets :: Integer
  , raffleCommitDeadline :: POSIXTime
  , raffleRevealDeadline :: POSIXTime
  , raffleTickets :: [RaffleTicket]
  , raffleParams :: RaffleParams
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleRedeemer
  = Buy PubKeyHash [BuiltinByteString]
  | Reveal [RaffleTicket]
  | Redeem
  | Cancel
  | CloseExpired
  | CloseUnderfunded
  | CloseUnrevealed
  | CloseExposedUnderfunded PubKeyHash
  | CloseExposedUnrevealed PubKeyHash
  deriving (Generic, FromJSON, ToJSON)

-- Generating ToData/FromData instances for the above types via Template Haskell
unstableMakeIsData ''RaffleTicket
unstableMakeIsData ''RaffleParams
makeIsDataIndexed ''RaffleDatum [('RaffleDatum, 0)]
makeIsDataIndexed
  ''RaffleRedeemer
  [ ('Buy, 1)
  , ('Reveal, 2)
  , ('Redeem, 3)
  , ('Cancel, 4)
  , ('CloseExpired, 5)
  , ('CloseUnderfunded, 6)
  , ('CloseUnrevealed, 7)
  , ('CloseExposedUnderfunded, 8)
  , ('CloseExposedUnrevealed, 9)
  ]

-- 2. Define Helper Functions & Lambda

raffleLamba :: RaffleValidatorParams -> RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
raffleLamba params raffle@RaffleDatum {..} redeemer context =
  let txInfo = scriptContextTxInfo context
      txSpendsStateToken =
        spendsToken raffleStateTokenAssetClass context
      stateTokenIsValid =
        "The raffleStateToken CurrencySymbol must be of the Raffle State Token Minting Policy"
          `traceIfFalse` raffleHasValidStateTokenCurrencySymbol params raffle
   in if stateTokenIsValid && txSpendsStateToken
        then case redeemer of
          Buy _pkh commits ->
            pand
              [ isInNewState raffle txInfo || isInCommitState raffle txInfo
              , raffleHasAvailableTickets raffle (plength commits)
              , ctxHasContinuingOutputWithCorrectValueAndDatum context raffle redeemer
              ]
          Reveal revealed_tickets ->
            pand
              [ isInRevealingState raffle txInfo
              , ctxHasContinuingOutputWithCorrectValueAndDatum context raffle redeemer
              , txIsSignedByTicketOwners context revealed_tickets
              ]
          Redeem ->
            pand
              []
          Cancel ->
            pand
              [ isInNewState raffle txInfo
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              ]
          CloseExpired ->
            pand
              [ isInExpiredState raffle txInfo
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              ]
          CloseUnderfunded ->
            pand
              [ isInUnderfundedState raffle txInfo
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              , txIsRefundingAllTickets txInfo raffle
              ]
          CloseExposedUnderfunded pkh ->
            pand
              [ isInUnderfundedExposedState raffle txInfo
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToPKH txInfo raffle pkh
              , txIsRefundingAllTickets txInfo raffle
              ]
          CloseUnrevealed ->
            pand
              [ isInUnrevealedState raffle txInfo
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              , txIsRefundingRevealedTickets txInfo raffle
              , txIsPayingUnrevealedValueToPKH txInfo raffle raffleOrganizer
              ]
          CloseExposedUnrevealed pkh ->
            pand
              [ isInUnrevealedExposedState raffle txInfo
              , txIsPayingPrizeToPKH txInfo raffle pkh
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsRefundingRevealedTickets txInfo raffle
              , txIsPayingUnrevealedValueToPKH txInfo raffle pkh
              ]
        else txIsPayingValueTo txInfo (getOwnInputValue context) (donationPKH raffleParams) --  Any invalid value sent to the raffle address should be spendable only to the raffle's donation PKH

---------------
---------------
-- UTILITIES --
---------------
---------------

-- | Helper function to check that the correct quantity of the given token is in a Value
isInValue :: (CurrencySymbol, TokenName, Integer) -> Value -> Bool
isInValue (cs, tn, q) = pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q #>= q') . flattenValue
{-# INLINEABLE isInValue #-}

-- | Helper function to check if a 'TxOut' contains exactly 1 quantity of an AssetClass
outHas1of :: TxOut -> AssetClass -> Bool
outHas1of (TxOut _ value _ _) (AssetClass (cs, tn)) = isInValue (cs, tn, 1) value
{-# INLINEABLE outHas1of #-}

-- | Helper function to check if a 'TxOut' contains a given datum and is inlined.
hasGivenInlineDatum :: ToData a => TxOut -> a -> Bool
hasGivenInlineDatum out datum = case txOutDatum out of
  OutputDatum da -> toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE hasGivenInlineDatum #-}

-- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
inputHas1of :: TxInInfo -> AssetClass -> Bool
inputHas1of = outHas1of . txInInfoResolved

-- | Helper function: check that the validating input contains a given token
spendsToken :: AssetClass -> ScriptContext -> Bool
spendsToken proofToken sc =
  "The transaction must spend the state token"
    `traceIfFalse` case (`inputHas1of` proofToken) #<$> findOwnInput sc of
      Nothing -> trace "Own input not found" False
      Just result -> traceIfFalse ("Token not spent: " #<> (decodeUtf8 . unTokenName . snd . unAssetClass $ proofToken)) result
{-# INLINEABLE spendsToken #-}

-- | Helper function to check that a UTxO is being spent in the transaction.
hasUtxo :: TxOutRef -> [TxInInfo] -> Bool
hasUtxo oref = pany (\(TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasUtxo #-}

tokenNameFromTxOutRef :: TxOutRef -> TokenName
tokenNameFromTxOutRef (TxOutRef (TxId txIdbs) txIdx) = TokenName (blake2b_256 (txIdbs #<> integerToBs txIdx))
{-# INLINEABLE tokenNameFromTxOutRef #-}

integerToBs :: Integer -> BuiltinByteString
integerToBs = serialiseData . mkI
{-# INLINEABLE integerToBs #-}

getOwnInputValue :: ScriptContext -> Value
getOwnInputValue context = case findOwnInput context of
  Nothing -> traceError "Own input not found"
  Just (TxInInfo _inOutRef inOut) -> txOutValue inOut
{-# INLINEABLE getOwnInputValue #-}

----------------------
----------------------
-- Raffle Tickets   --
----------------------
----------------------

{- |  This function receives a 'PubKeyHash', a 'BuiltinByteString' represeting a secret hash and a '[RaffleTicket]'
, and returns a new list including a new ticket with 'PubKeyHash' as owner.
-}
insertTicket :: PubKeyHash -> BuiltinByteString -> [RaffleTicket] -> [RaffleTicket]
insertTicket pkh secrethash tickets =
  let count = plength tickets
   in RaffleTicket pkh count secrethash Nothing : tickets

isValidTicket :: RaffleTicket -> Bool
isValidTicket RaffleTicket {ticketSecret, ticketSecretHash} = case ticketSecret of
  Nothing -> False
  Just ts -> sha2_256 ts #== ticketSecretHash

{- | This function receives two 'RaffleTicket' and returns 'True' if the following conditions are met.
     * both tickets have the same number
     * both tickets have the same secret hash.
     * the second ticket is unrevealed.
     * the first ticket is valid.
-}
isRevealedOf :: RaffleTicket -> RaffleTicket -> Bool
isRevealedOf new old =
  pand
    [ ticketNumber new #== ticketNumber old
    , ticketSecretHash new #== ticketSecretHash old
    , isNothing (ticketSecret old)
    , isValidTicket new
    ]

-- | This function receives two 'RaffleTicket' and returns the first one if 'isRevealedOf' the second one, otherwise it returns the second one.
reavealIfMatched :: RaffleTicket -> RaffleTicket -> RaffleTicket
reavealIfMatched new ticket = if new `isRevealedOf` ticket then new else ticket

{- | This function receives a ''RaffleTicket', and a '[RaffleTicket]'
, and returns a new list with the corresponding ticket revealed.
-}
reavealSecret :: RaffleTicket -> [RaffleTicket] -> [RaffleTicket]
reavealSecret new tickets = reavealIfMatched new <$> tickets

updateRaffle :: RaffleRedeemer -> RaffleDatum -> RaffleDatum
updateRaffle (Buy pkh commits) datum@RaffleDatum {raffleTickets} =
  let updatedListOfTickets = pfoldr (insertTicket pkh) raffleTickets commits
   in datum {raffleTickets = updatedListOfTickets}
updateRaffle (Reveal newtickets) datum@RaffleDatum {raffleTickets} =
  let updatedListOfTickets = pfoldr reavealSecret raffleTickets newtickets
   in datum {raffleTickets = updatedListOfTickets}
updateRaffle _ _ = error "invalid redeemer to update datum"

----------------------
----------------------
-- Raffle Functions --
----------------------
----------------------

-- | This is a function which determines the @revealing interval@ for a given 'RaffleDatum'.
getRaffleRevealingValidRange :: RaffleDatum -> POSIXTimeRange
getRaffleRevealingValidRange RaffleDatum {..} = Interval (lowerBound raffleCommitDeadline) (upperBound raffleRevealDeadline)

getRaffleCloseUnderfundedValidRange :: RaffleDatum -> POSIXTimeRange
getRaffleCloseUnderfundedValidRange RaffleDatum {..} = Interval (lowerBound raffleCommitDeadline) (upperBound (raffleCommitDeadline #+ fromInteger (closingWindow raffleParams)))

getRaffleCloseUnrevealedValidRange :: RaffleDatum -> POSIXTimeRange
getRaffleCloseUnrevealedValidRange RaffleDatum {..} = Interval (lowerBound raffleRevealDeadline) (upperBound (raffleRevealDeadline #+ fromInteger (closingWindow raffleParams)))

getRaffleAccumulatedValue :: RaffleDatum -> Value
getRaffleAccumulatedValue RaffleDatum {..} = lovelaceValueOf (raffleTicketPrice * plength raffleTickets)

getRaffleUnrevealedTickets :: RaffleDatum -> [RaffleTicket]
getRaffleUnrevealedTickets RaffleDatum {..} = filter (isNothing . ticketSecret) raffleTickets

getUnrevealedValue :: RaffleDatum -> Value
getUnrevealedValue raffle =
  let unRevealedTickets = getRaffleUnrevealedTickets raffle
   in lovelaceValueOf (raffleTicketPrice raffle #* plength unRevealedTickets)

-- -- TO DO
-- determineRaffleWinner :: RaffleDatum -> PubKeyHash
-- determineRaffleWinner = raffleOrganizer

-- | This ensures the link between state token minting policy and current validator
raffleHasValidStateTokenCurrencySymbol :: RaffleValidatorParams -> RaffleDatum -> Bool
raffleHasValidStateTokenCurrencySymbol (RaffleValidatorParams (MintingPolicyHash csbs)) RaffleDatum {..} =
  csbs #== (unCurrencySymbol . fst . unAssetClass) raffleStateTokenAssetClass
{-# INLINEABLE raffleHasValidStateTokenCurrencySymbol #-}

raffleHasAvailableTickets :: RaffleDatum -> Integer -> Bool
raffleHasAvailableTickets RaffleDatum {raffleParams, raffleTickets} noOfNewTickets =
  "The maximum no. of tickets must not be reached"
    `traceIfFalse` ((plength raffleTickets #+ noOfNewTickets) #<= maxNoOfTickets raffleParams)

------------------
------------------
-- Check States --
------------------
------------------

{- | This is a function to check that a RaffleDatum represents a raffle in a @New State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline have not passed.
     * The tickets list must be empty.
-}
isInNewState :: RaffleDatum -> TxInfo -> Bool
isInNewState RaffleDatum {raffleTickets, raffleCommitDeadline} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid new state"
    `traceIfFalse` pand
      [ "tx valid range is not before commit deadline"
          `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
      , "raffleTickets is not empty"
          `traceIfFalse` pnull raffleTickets
      ]
{-# INLINEABLE isInNewState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in a @Expired State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * The tickets list must be empty.
-}
isInExpiredState :: RaffleDatum -> TxInfo -> Bool
isInExpiredState RaffleDatum {raffleTickets, raffleCommitDeadline} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid expired state"
    `traceIfFalse` pand
      [ "tx valid range is not after commit deadline"
          `traceIfFalse` (raffleCommitDeadline `before` txInfoValidRange)
      , "raffleTickets is not empty"
          `traceIfFalse` pnull raffleTickets
      ]

{- | This is a function to check that a RaffleDatum represents a raffle in a @Committing State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline have not passed.
     * At least one ticket have been bought for the current raffle.
-}
isInCommitState :: RaffleDatum -> TxInfo -> Bool
isInCommitState RaffleDatum {raffleTickets, raffleCommitDeadline} TxInfo {txInfoValidRange} =
  pand
    [ "tx valid range is not before commit deadline"
        `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
    , "raffleTickets must not be empty " `traceIfFalse` (not . pnull) raffleTickets
    ]
{-# INLINEABLE isInCommitState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in an @Underfunded State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * Tickets have been bought.
     * The minimum amount was not reached.
     * The closing deadline (closing window after committing deadline) has not passed.
-}
isInUnderfundedState :: RaffleDatum -> TxInfo -> Bool
isInUnderfundedState raffle@RaffleDatum {..} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid underfunded state"
    `traceIfFalse` pand
      [ "tx valid range must be between commit deadline and commit deadline + closing widnow"
          `traceIfFalse` (getRaffleCloseUnderfundedValidRange raffle `contains` txInfoValidRange)
      , "raffleTickets must not be empty "
          `traceIfFalse` (not . pnull) raffleTickets
      , "sold tickets must be less than min. no. of tickets"
          `traceIfFalse` ((#< raffleMinNoOfTickets) . plength $ raffleTickets)
      ]
{-# INLINEABLE isInUnderfundedState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in an @Underfunded Exposed State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * Tickets have been bought.
     * The minimum amount was not reached.
     * The closing deadline (closing window after committing deadline) has passed.
-}
isInUnderfundedExposedState :: RaffleDatum -> TxInfo -> Bool
isInUnderfundedExposedState RaffleDatum {..} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid underfunded exposed state"
    `traceIfFalse` pand
      [ "tx valid range must be after commit deadline + closing widnow"
          `traceIfFalse` ((raffleCommitDeadline #+ fromInteger (closingWindow raffleParams)) `before` txInfoValidRange)
      , "raffleTickets must not be empty "
          `traceIfFalse` (not . pnull) raffleTickets
      , "sold tickets must be less than min. no. of tickets"
          `traceIfFalse` ((#< raffleMinNoOfTickets) . plength $ raffleTickets)
      ]
{-# INLINEABLE isInUnderfundedExposedState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in a @Revealing State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * The minimum number of tickets to be sold was reached. (which implies that tickets have been bought)
     * Not all tickets were revealed yet.
     * The revealing deadline have not been passed.
-}
isInRevealingState :: RaffleDatum -> TxInfo -> Bool
isInRevealingState datum@RaffleDatum {..} TxInfo {..} =
  "The raffle must be in a valid revealing state"
    `traceIfFalse` pand
      [ "not in revealing interval"
          `traceIfFalse` (getRaffleRevealingValidRange datum `contains` txInfoValidRange)
      , "min. no. of tickets not met"
          `traceIfFalse` (plength raffleTickets #>= raffleMinNoOfTickets)
      , "no unrevealed tickets found"
          `traceIfFalse` pany (isNothing . ticketSecret) raffleTickets
      ]
{-# INLINEABLE isInRevealingState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in a @Unrevealed State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The revealing deadline has passed.
     * The minimum number of tickets to be sold was reached. (which implies that tickets have been bought)
     * Not all ticket secrets where revealed.
     * The closing deadline (closing window after revealing deadline) have not passed.
-}
isInUnrevealedState :: RaffleDatum -> TxInfo -> Bool
isInUnrevealedState raffle@RaffleDatum {..} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid unrevealed state"
    `traceIfFalse` pand
      [ "tx valid range must be between reveal deadline and reveal deadline + closing widnow"
          `traceIfFalse` (getRaffleCloseUnrevealedValidRange raffle `contains` txInfoValidRange)
      , "unrevealed tickets must exists"
          `traceIfFalse` pany (isNothing . ticketSecret) raffleTickets
      , "min. no. of tickets must have been sold"
          `traceIfFalse` ((#>= raffleMinNoOfTickets) . plength $ raffleTickets)
      ]
{-# INLINEABLE isInUnrevealedState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in a @Unrevealed State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The revealing deadline has passed.
     * The minimum number of tickets to be sold was reached. (which implies that tickets have been bought)
     * Not all ticket secrets where revealed.
     * The closing deadline (closing window after revealing deadline) has passed.
-}
isInUnrevealedExposedState :: RaffleDatum -> TxInfo -> Bool
isInUnrevealedExposedState raffle@RaffleDatum {..} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid unrevealed  exposed state"
    `traceIfFalse` pand
      [ "tx valid range must be after reveal deadline + closing widnow"
          `traceIfFalse` ((raffleRevealDeadline #+ fromInteger (closingWindow raffleParams)) `before` txInfoValidRange)
      , "unrevealed tickets must exists"
          `traceIfFalse` (not . pnull) (getRaffleUnrevealedTickets raffle)
      , "min. no. of tickets must have been sold"
          `traceIfFalse` ((#>= raffleMinNoOfTickets) . plength $ raffleTickets)
      ]

{- | This is a function to check that a RaffleDatum represents a raffle in a @Winner Selected By CRS State@, in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline should not have been passed.
     * At least one ticket have been bought for the current raffle.
-}
isWinnerSelectedByCRS :: RaffleDatum -> TxInfo -> Bool
isWinnerSelectedByCRS RaffleDatum {..} TxInfo {txInfoValidRange} =
  "The raffle must be in a valid Winner Selected by CRS state"
    `traceIfFalse` pand
      [ "tx valid range must be after revealing deadline" `traceIfFalse` (raffleRevealDeadline `before` txInfoValidRange)
      , "all tickets must be revealed" `traceIfFalse` pall (isJust . ticketSecret) raffleTickets
      ]
{-# INLINEABLE isWinnerSelectedByCRS #-}

------------------
------------------
-- RAFFLE IN TX --
------------------
------------------

txIsBurningStateToken :: TxInfo -> RaffleDatum -> Bool
txIsBurningStateToken TxInfo {txInfoMint} RaffleDatum {raffleStateTokenAssetClass} =
  let AssetClass (policyCurrencySymbol, stateTokenName) = raffleStateTokenAssetClass
   in isInValue (policyCurrencySymbol, stateTokenName, -1) txInfoMint

txIsPayingValueTo :: TxInfo -> Value -> PubKeyHash -> Bool
txIsPayingValueTo txInfo value pkh =
  let txOutsToPKH = filter ((#== pubKeyHashAddress pkh) . txOutAddress) (txInfoOutputs txInfo)
      paidValue = psum (txOutValue #<$> txOutsToPKH)
   in paidValue `geq` value

txIsPayingPrizeToPKH :: TxInfo -> RaffleDatum -> PubKeyHash -> Bool
txIsPayingPrizeToPKH txInfo RaffleDatum {..} pkh =
  pand
    [ "The transaction must pay the prize back to the pkh"
        `traceIfFalse` txIsPayingValueTo txInfo rafflePrizeValue pkh
    ]

txIsPayingPrizeToOrganizer :: TxInfo -> RaffleDatum -> Bool
txIsPayingPrizeToOrganizer txInfo RaffleDatum {..} =
  pand
    [ "The transaction must be signed by the raffle organizer"
        `traceIfFalse` txSignedBy txInfo raffleOrganizer
    , "The transaction must pay the prize back to the raffle organizer"
        `traceIfFalse` txIsPayingValueTo txInfo rafflePrizeValue raffleOrganizer
    ]

txIsPayingPriceToTicketOwner :: TxInfo -> Integer -> RaffleTicket -> Bool
txIsPayingPriceToTicketOwner txInfo price RaffleTicket {..} = txIsPayingValueTo txInfo (lovelaceValueOf price) ticketOwner

txIsRefundingAllTickets :: TxInfo -> RaffleDatum -> Bool
txIsRefundingAllTickets txInfo RaffleDatum {..} =
  "The transaction must pay the ticket price back to each ticket owner."
    `traceIfFalse` pall (txIsPayingPriceToTicketOwner txInfo raffleTicketPrice) raffleTickets

txIsRefundingRevealedTickets :: TxInfo -> RaffleDatum -> Bool
txIsRefundingRevealedTickets txInfo RaffleDatum {..} =
  "The transaction must pay the ticket price back to each ticket owner who revealed the ticket secret."
    `traceIfFalse` pall (txIsPayingPriceToTicketOwner txInfo raffleTicketPrice) (filter (isJust . ticketSecret) raffleTickets)

-- | This is a function which checks if a given transaction is signed by each ticket owner.
txIsSignedByTicketOwners :: ScriptContext -> [RaffleTicket] -> Bool
txIsSignedByTicketOwners ScriptContext {scriptContextTxInfo} tickets =
  "The transaction must be signed by each ticket owner"
    `traceIfFalse` pall ((scriptContextTxInfo `txSignedBy`) . ticketOwner) tickets

txIsPayingUnrevealedValueToPKH :: TxInfo -> RaffleDatum -> PubKeyHash -> Bool
txIsPayingUnrevealedValueToPKH txInfo datum pkh =
  "The transaction must pay the amount for the unrevealed tickets to the pkh"
    `traceIfFalse` txIsPayingValueTo txInfo (getUnrevealedValue datum) pkh

-----------------------
-----------------------
-- RAFFLE IN CONTEXT --
-----------------------
-----------------------

ctxGetExtraAmount :: ScriptContext -> RaffleDatum -> Value
ctxGetExtraAmount context raffle@RaffleDatum {..} =
  getOwnInputValue context
    #- ( getRaffleAccumulatedValue raffle
          #+ rafflePrizeValue
          #+ assetClassValue raffleStateTokenAssetClass 1
       )

ctxIsPayingDonation :: ScriptContext -> RaffleDatum -> Bool
ctxIsPayingDonation context@ScriptContext {scriptContextTxInfo} raffle@RaffleDatum {..} = txIsPayingValueTo scriptContextTxInfo (ctxGetExtraAmount context raffle) (donationPKH raffleParams)

ctxIsBurningStateTokenAndPayingAnyExtraToDonation :: ScriptContext -> RaffleDatum -> Bool
ctxIsBurningStateTokenAndPayingAnyExtraToDonation context@ScriptContext {scriptContextTxInfo} raffle =
  pand
    [ traceIfFalse
        "The transaction must burn the raffleStateToken"
        (txIsBurningStateToken scriptContextTxInfo raffle)
    , traceIfFalse
        "The transaction must pay any excess value to the donation pkh"
        (ctxIsPayingDonation context raffle)
    ]

-- | The transaction must have an output containing the previous value locked with state token, and with the updated datum inlined, locked at the raffle validator's address.
ctxHasContinuingOutputWithCorrectValueAndDatum :: ScriptContext -> RaffleDatum -> RaffleRedeemer -> Bool
ctxHasContinuingOutputWithCorrectValueAndDatum context datum redeemer =
  let txOutsToRaffleValidator = getContinuingOutputs context
      updatedDatum = updateRaffle redeemer datum
      valueOfNewTickets = \old new ->
        lovelaceValueOf $
          (plength . raffleTickets) new #- (plength . raffleTickets) old
      newTicketsValue = valueOfNewTickets datum updatedDatum
      outHasCorrectValueWithDatum =
        ( \out ->
            pand
              [ txOutValue out `geq` (getOwnInputValue context #+ newTicketsValue)
              , out `hasGivenInlineDatum` updatedDatum
              ]
        )
   in traceIfFalse
        "No valid continuing output found !"
        (pany outHasCorrectValueWithDatum txOutsToRaffleValidator)
{-# INLINEABLE ctxHasContinuingOutputWithCorrectValueAndDatum #-}

------------------------------------
--------
--------
--------
--------
--------
--------
--------
--------
--------
--------
--------
--------
--------
------------------------------------

-- | Untyped version of the spending validator lambda.
untypedLambda :: RaffleValidatorParams -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator . raffleLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type RaffleValidator = ValidatorContract "raffle"

-- | Function for producing the compiled spending validator script.
compileValidator :: RaffleValidatorParams -> RaffleValidator
compileValidator params = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode params)
