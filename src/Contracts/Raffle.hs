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
raffleLamba params datum@RaffleDatum {..} redeemer context =
  let txInfo = scriptContextTxInfo context
      hasStateToken =
        "The transaction must spend the state token"
          `traceIfFalse` spendsToken raffleStateTokenAssetClass context
      stateTokenIsValid =
        "The raffleStateToken CurrencySymbol must be of the Raffle State Token Minting Policy"
          `traceIfFalse` raffleHasValidStateTokenCurrencySymbol params datum
   in if stateTokenIsValid && hasStateToken
        then case redeemer of
          Buy _pkh commits ->
            pand
              [ "The transaction valid range shoud be before the commit deadline"
                  `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange txInfo)
              , "The maximum no. of tickets must not be reached"
                  `traceIfFalse` ((plength raffleTickets #+ plength commits) #<= maxNoOfTickets raffleParams)
              , "The transaction must have an output containing the previous value locked with state token, and with the updated datum inlined, locked at the raffle validator's address."
                  `traceIfFalse` txHasContinuingOutputWithCorrectValueAndDatum datum redeemer context
              ]
          Reveal revealed_tickets ->
            pand
              [ "The raffle must be in a valid revealing state"
                  `traceIfFalse` isInRevealingState datum txInfo
              , "The transaction must have an output containing the previous value locked with state token, and with the updated datum inlined, locked at the raffle validator's address."
                  `traceIfFalse` txHasContinuingOutputWithCorrectValueAndDatum datum redeemer context
              , "The transaction must be signed by each ticket owner"
                  `traceIfFalse` isSignedByTicketOwners txInfo revealed_tickets
              ]
          Redeem -> False
          Cancel ->
            pand
              [ "The raffle must be in a valid new state"
                  `traceIfFalse` isInNewState datum txInfo
              , "The transaction must be signed by the raffle organizer"
                  `traceIfFalse` txSignedBy txInfo raffleOrganizer
              , "The transaction must burn the raffleStateToken"
                  `traceIfFalse` burnStateToken datum context
              , "The transaction must pay any excess value to the donation pkh"
                  `traceIfFalse` isPayingDonation context datum
              , "The transaction must pay the prize back to the raffle organizer"
                  `traceIfFalse` isPayingValueTo context rafflePrizeValue raffleOrganizer
              ]
          CloseExpired ->
            pand
              [ "The raffle must be in a valid expired state"
                  `traceIfFalse` isInExpiredState datum txInfo
              , "The transaction must be signed by the raffle organizer"
                  `traceIfFalse` txSignedBy txInfo raffleOrganizer
              , "The transaction must burn the raffleStateToken"
                  `traceIfFalse` burnStateToken datum context
              , "The transaction must pay any excess value to the donation pkh"
                  `traceIfFalse` isPayingDonation context datum
              , "The transaction must pay the prize back to the raffle organizer"
                  `traceIfFalse` isPayingValueTo context rafflePrizeValue raffleOrganizer
              ]
          CloseUnderfunded ->
            pand
              [ "The raffle must be in a valid underfunded state"
                  `traceIfFalse` isInUnderfundedState datum txInfo
              , "The transaction must be signed by the raffle organizer"
                  `traceIfFalse` txSignedBy txInfo raffleOrganizer
              , "The transaction must burn the raffleStateToken"
                  `traceIfFalse` burnStateToken datum context
              , "The transaction must pay any excess value to the donation pkh"
                  `traceIfFalse` isPayingDonation context datum
              , "The transaction must pay the prize back to the raffle organizer"
                  `traceIfFalse` isPayingValueTo context rafflePrizeValue raffleOrganizer
              , "The transaction must pay the ticket price back to each ticket owner."
                  `traceIfFalse` isRefundingAllTickets datum context
              ]
          CloseUnrevealed -> False
          CloseExposedUnderfunded _pkh -> False
          CloseExposedUnrevealed _pkh -> False
        else isPayingValueTo context (getOwnInputValue context) (donationPKH raffleParams) --  Only goes to donation PKH

-- | This ensures the link between state token minting policy and current validator
raffleHasValidStateTokenCurrencySymbol :: RaffleValidatorParams -> RaffleDatum -> Bool
raffleHasValidStateTokenCurrencySymbol (RaffleValidatorParams (MintingPolicyHash csbs)) RaffleDatum {..} =
  csbs #== (unCurrencySymbol . fst . unAssetClass) raffleStateTokenAssetClass
{-# INLINEABLE raffleHasValidStateTokenCurrencySymbol #-}

------------------BUY RAFFLE -----------

-- Checks if a given txout has a value greater than the one received as param and has an inline datum
hasValueWithDatum :: TxOut -> Value -> RaffleDatum -> Bool
hasValueWithDatum out value datum =
  pand
    [ txOutValue out `geq` value
    , out `hasGivenInlineDatum` datum
    ]
{-# INLINEABLE hasValueWithDatum #-}

txHasContinuingOutputWithCorrectValueAndDatum :: RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
txHasContinuingOutputWithCorrectValueAndDatum datum redeemer context =
  let txOutsToRaffleValidator = getContinuingOutputs context
      updatedDatum = updateRaffle redeemer datum
      valueOfNewTickets = \old new ->
        lovelaceValueOf $
          (plength . raffleTickets) new #- (plength . raffleTickets) old
      newTicketsValue = valueOfNewTickets datum updatedDatum
      hasCorrectValueWithDatum = (`hasValueWithDatum` (getOwnInputValue context #+ newTicketsValue))
   in pany (`hasCorrectValueWithDatum` updatedDatum) txOutsToRaffleValidator
{-# INLINEABLE txHasContinuingOutputWithCorrectValueAndDatum #-}

-----------------------------------------------

{- | This is a function to check that a RaffleDatum represents a raffle in a @New State@ in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline should not have been passed.
     * No tickets should have been bought yet for the current raffle.
-}
isInNewState :: RaffleDatum -> TxInfo -> Bool
isInNewState RaffleDatum {raffleTickets, raffleCommitDeadline} TxInfo {txInfoValidRange} =
  pand
    [ "tx valid range is not before commit deadline"
        `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
    , "raffleTickets is not empty"
        `traceIfFalse` pnull raffleTickets
    ]
{-# INLINEABLE isInNewState #-}

-----------

{- | This is a function to check that a RaffleDatum represents a raffle in a @Expired State@ in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * No tickets have been bought.
-}
isInExpiredState :: RaffleDatum -> TxInfo -> Bool
isInExpiredState RaffleDatum {raffleTickets, raffleCommitDeadline} TxInfo {txInfoValidRange} =
  pand
    [ "tx valid range is not after commit deadline" `traceIfFalse` (raffleCommitDeadline `before` txInfoValidRange)
    , "raffleTickets is not empty" `traceIfFalse` pnull raffleTickets
    ]

-----------------------------------------
------------------BUY TICKETS -----------

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

-----------------------------------------
------------------REVEAL  TICKETS -----------

-- | This is a function which determines the @revealing interval@ for a given 'RaffleDatum'.
getRevealingInterval :: RaffleDatum -> Interval POSIXTime
getRevealingInterval RaffleDatum {..} = Interval (LowerBound (Finite raffleCommitDeadline) False) (UpperBound (Finite raffleRevealDeadline) False)

{- | This is a function to check that a RaffleDatum represents a raffle in a @Revealing State@ in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * The revealing deadline have not been passed.
     * The minimum number of tickets to be sold was reached.
     * Not all tickets were revealed yet.
-}
isInRevealingState :: RaffleDatum -> TxInfo -> Bool
isInRevealingState datum@RaffleDatum {..} TxInfo {..} =
  pand
    [ getRevealingInterval datum `contains` txInfoValidRange
    , plength raffleTickets #>= raffleMinNoOfTickets
    , pany (isNothing . ticketSecret) raffleTickets
    ]

-- | This is a function which checks if a given transaction is signed by each ticket owner.
isSignedByTicketOwners :: TxInfo -> [RaffleTicket] -> Bool
isSignedByTicketOwners txInfo = pall ((txInfo `txSignedBy`) . ticketOwner)

-----------------------------------------
------------------Cancel -----------

getRaffleAccumulatedValue :: RaffleDatum -> Value
getRaffleAccumulatedValue RaffleDatum {..} = lovelaceValueOf (raffleTicketPrice * plength raffleTickets)

getDonationAmount :: ScriptContext -> RaffleDatum -> Value
getDonationAmount context raffle@RaffleDatum {..} =
  getOwnInputValue context
    #- ( getRaffleAccumulatedValue raffle
          #+ rafflePrizeValue
          #+ assetClassValue raffleStateTokenAssetClass 1
       )

getUnrevealedValue :: RaffleDatum -> Value
getUnrevealedValue RaffleDatum {..} =
  let unRevealedTickets = filter (isNothing . ticketSecret) raffleTickets
   in lovelaceValueOf (raffleTicketPrice * plength unRevealedTickets)

isPayingDonation :: ScriptContext -> RaffleDatum -> Bool
isPayingDonation context raffle@RaffleDatum {..} = isPayingValueTo context (getDonationAmount context raffle) (donationPKH raffleParams)

isPayingValueTo :: ScriptContext -> Value -> PubKeyHash -> Bool
isPayingValueTo ScriptContext {scriptContextTxInfo} value pkh =
  let txOutsToPKH = filter ((#== pubKeyHashAddress pkh) . txOutAddress) (txInfoOutputs scriptContextTxInfo)
      paidValue = psum (txOutValue #<$> txOutsToPKH)
   in paidValue `geq` value

-- TO DO
determineRaffleWinner :: RaffleDatum -> PubKeyHash
determineRaffleWinner = raffleOrganizer

burnStateToken :: RaffleDatum -> ScriptContext -> Bool
burnStateToken RaffleDatum {raffleStateTokenAssetClass} (ScriptContext TxInfo {txInfoMint} _) =
  let AssetClass (policyCurrencySymbol, stateTokenName) = raffleStateTokenAssetClass
   in isInValue (policyCurrencySymbol, stateTokenName, -1) txInfoMint

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
spendsToken proofToken sc = case (`inputHas1of` proofToken) #<$> findOwnInput sc of
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

------------------------------------
------------------Underfunded -----------

{- | This is a function to check that a RaffleDatum represents a raffle in an @Underfunded State@ in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * Tickets have been bought.
     * The minimum amount was not reached.
-}
isInUnderfundedState :: RaffleDatum -> TxInfo -> Bool
isInUnderfundedState RaffleDatum {..} TxInfo {txInfoValidRange} =
  pand
    [ "tx valid range is not after commit deadline" `traceIfFalse` (raffleCommitDeadline `before` txInfoValidRange)
    , "raffleTickets is empty" `traceIfFalse` (not . pnull) raffleTickets
    , "not underfunded" `traceIfFalse` ((#< raffleMinNoOfTickets) . plength $ raffleTickets)
    ]

isPayingPriceToTicketOwner :: ScriptContext -> Integer -> RaffleTicket -> Bool
isPayingPriceToTicketOwner context price RaffleTicket {..} = isPayingValueTo context (lovelaceValueOf price) ticketOwner

isRefundingAllTickets :: RaffleDatum -> ScriptContext -> Bool
isRefundingAllTickets RaffleDatum {..} context = pall (isPayingPriceToTicketOwner context raffleTicketPrice) raffleTickets

-----------
-----------
-- --------
--------
--------
--------
--------
---------

-- | Untyped version of the spending validator lambda.
untypedLambda :: RaffleValidatorParams -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator . raffleLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type RaffleValidator = ValidatorContract "raffle"

-- | Function for producing the compiled spending validator script.
compileValidator :: RaffleValidatorParams -> RaffleValidator
compileValidator params = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode params)
