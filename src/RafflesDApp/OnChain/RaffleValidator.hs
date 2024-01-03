{-# OPTIONS_HADDOCK show-extensions #-}

module RafflesDApp.OnChain.RaffleValidator where

import Jambhala.Plutus hiding (Address)
import Jambhala.Utils
import Plutus.V1.Ledger.Interval (after, before)
import Plutus.V1.Ledger.Value (geq)
import Plutus.V2.Ledger.Api
import PlutusTx.Eq qualified as PlutusTx

-- *  Types

-- ** Script Parameters
newtype RaffleValidatorParams = RaffleValidatorParams
  {raffleStateTokenPolicy :: MintingPolicyHash}
  deriving (Generic, FromJSON, ToJSON)

--  Generate Lift instance
makeLift ''RaffleValidatorParams

---- ** Custom data types

data RaffleParams = RaffleParams
  { -- | The milliseconds after commit or reaveal deadline in which only the organizer can close underfunded or unrevealed raffles.
    closingWindow :: Integer
  , -- | The minimum difference between the commit and reveal deadlines in milliseconds.
    minRevealingWindow :: Integer
  , -- | The 'PubKeyHash' to which any extra amount locked at the raffle validator's address must be locked when the raffle is closed.
    donationPKH :: PubKeyHash
  , -- | The maximum number of tickets a raffle can have. This parameter is in place to prevent exceeding transaction size on refunds.
    maxNoOfTickets :: Integer
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

makeLift ''RaffleParams
unstableMakeIsData ''RaffleParams

data RaffleTicket = RaffleTicket
  { ticketOwner :: PubKeyHash
  , ticketNumber :: Integer
  , ticketSecretHash :: BuiltinByteString
  , ticketSecret :: Maybe BuiltinByteString
  }
  deriving (Generic, FromJSON, ToJSON)

unstableMakeIsData ''RaffleTicket

data RaffleDatum = RaffleDatum
  { -- | linked between mp and validator
    raffleParams :: RaffleParams
  , -- | linked between mp and validator
    raffleStateTokenAssetClass :: AssetClass
  , raffleOrganizer :: PubKeyHash
  , rafflePrizeValue :: Value
  , raffleTicketPrice :: Integer
  , raffleMinNoOfTickets :: Integer
  , raffleCommitDeadline :: POSIXTime
  , raffleRevealDeadline :: POSIXTime
  , raffleTickets :: [RaffleTicket]
  }
  deriving (Generic, FromJSON, ToJSON)

makeIsDataIndexed ''RaffleDatum [('RaffleDatum, 0)]

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

-- *  Validator Lambda

raffleLamba :: RaffleValidatorParams -> RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
raffleLamba params raffle@RaffleDatum {..} redeemer context =
  let txInfo = scriptContextTxInfo context
      txValidRange = txInfoValidRange txInfo
      txSpendsStateToken =
        spendsToken raffleStateTokenAssetClass context
      stateTokenIsValid =
        "The raffleStateToken CurrencySymbol must be of the Raffle State Token Minting Policy"
          `traceIfFalse` raffleHasValidStateTokenCurrencySymbol params raffle
   in if stateTokenIsValid && txSpendsStateToken
        then case redeemer of
          Buy _pkh commits ->
            pand
              [ isInNewState raffle txValidRange || isInCommitState raffle txValidRange
              , raffleHasAvailableTickets raffle (plength commits)
              , ctxHasContinuingOutputWithCorrectValueAndDatum context raffle redeemer
              ]
          Reveal revealed_tickets ->
            pand
              [ isInRevealingState raffle txValidRange
              , ctxHasContinuingOutputWithCorrectValueAndDatum context raffle redeemer
              , txIsSignedByTicketOwners context revealed_tickets
              ]
          Redeem ->
            pand
              [ isWinnerSelectedByCRS raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingAccumulatedValueToOrganizer txInfo raffle
              , txIsPayingPrizeToPKH txInfo raffle (determineRaffleWinner raffle)
              ]
          Cancel ->
            pand
              [ isInNewState raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              ]
          CloseExpired ->
            pand
              [ isInExpiredState raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              ]
          CloseUnderfunded ->
            pand
              [ isInUnderfundedState raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              , txIsRefundingAllTickets txInfo raffle
              ]
          CloseExposedUnderfunded pkh ->
            pand
              [ isInUnderfundedExposedState raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToPKH txInfo raffle pkh
              , txIsRefundingAllTickets txInfo raffle
              ]
          CloseUnrevealed ->
            pand
              [ isInUnrevealedState raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToOrganizer txInfo raffle
              , txIsRefundingRevealedTickets txInfo raffle
              , txIsPayingUnrevealedValueToPKH txInfo raffle raffleOrganizer
              ]
          CloseExposedUnrevealed pkh ->
            pand
              [ isInUnrevealedExposedState raffle txValidRange
              , ctxIsBurningStateTokenAndPayingAnyExtraToDonation context raffle
              , txIsPayingPrizeToPKH txInfo raffle pkh
              , txIsRefundingRevealedTickets txInfo raffle
              , txIsPayingUnrevealedValueToPKH txInfo raffle pkh
              ]
        else txIsPayingValueTo txInfo (getOwnInputValue context) (donationPKH raffleParams) --  Any invalid value sent to the raffle address should be spendable only to the raffle's donation PKH

------------------------

-- **  Helper Functions

------------------------

integerToBs :: Integer -> BuiltinByteString
integerToBs x = integerToBSHelper (if x #< 0 then pnegate x else x) (x #< 0) emptyByteString
  where
    integerToBSHelper :: Integer -> Bool -> BuiltinByteString -> BuiltinByteString
    integerToBSHelper x' isNegative acc -- quotient is 0 means x is single-digit
      | x' #== 0 && isNegative = consByteString 45 acc -- prepend '-' for negative numbers
      | x' #== 0 && isNegative #== False = acc
      | otherwise =
        let (q, r) = x' `quotRem` 10
         in integerToBSHelper q isNegative (digitToBS r #<> acc)

    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (48 #+ fromInteger d) emptyByteString -- 48 is ASCII code for '0'
{-# INLINEABLE integerToBs #-}

bsToInteger :: BuiltinByteString -> Integer
bsToInteger bs | bs #== emptyByteString = 0
bsToInteger bs =
  let (isNegative, bsTrimmed) =
        if indexByteString bs 0 #== 45 -- ASCII code for '-'
          then (True, dropByteString 1 bs)
          else (False, bs)
   in bytesToInteger $ bsToIntegerHelper bsTrimmed []
{-# INLINEABLE bsToInteger #-}

bsToIntegerHelper :: BuiltinByteString -> [Integer] -> [Integer]
bsToIntegerHelper bs acc | bs #== emptyByteString = acc
bsToIntegerHelper bs acc =
  let digit = indexByteString bs 0
      newAcc = digit : acc
   in bsToIntegerHelper (dropByteString 1 bs) newAcc
{-# INLINEABLE bsToIntegerHelper #-}

bytesToInteger :: [Integer] -> Integer
bytesToInteger bytes =
  let bytesToIntegerHelper [] acc _ = acc
      bytesToIntegerHelper (b : bs) acc shiftAmount
        | b #< 0 || b #> 255 = traceError "Invalid byte value"
        | otherwise = bytesToIntegerHelper bs (acc #+ (b #* (2 `raisedTo` shiftAmount))) (shiftAmount #+ 8)
   in bytesToIntegerHelper bytes 0 0
{-# INLINEABLE bytesToInteger #-}

raisedTo :: Integer -> Integer -> Integer
raisedTo _x y | y #== 0 = 1
raisedTo x y | y #== 1 = x
raisedTo x y = x #* raisedTo x (y #- 1)
{-# INLINEABLE raisedTo #-}

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
tokenNameFromTxOutRef (TxOutRef (TxId txIdbs) txIdx) = TokenName (sha2_256 (txIdbs #<> integerToBs txIdx))
{-# INLINEABLE tokenNameFromTxOutRef #-}

getOwnInputValue :: ScriptContext -> Value
getOwnInputValue context = case findOwnInput context of
  Nothing -> traceError "Own input not found"
  Just (TxInInfo _inOutRef inOut) -> txOutValue inOut
{-# INLINEABLE getOwnInputValue #-}

------------------------

-- **  Raffle Tickets Functions

------------------------

{- |  This function receives a 'PubKeyHash', a 'BuiltinByteString' represeting a secret hash and a '[RaffleTicket]'
, and returns a new list including a new ticket with 'PubKeyHash' as owner.
-}
insertTicket :: PubKeyHash -> BuiltinByteString -> [RaffleTicket] -> [RaffleTicket]
insertTicket pkh secrethash tickets =
  let count = plength tickets
   in RaffleTicket pkh count secrethash Nothing : tickets
{-# INLINEABLE insertTicket #-}

isValidTicket :: RaffleTicket -> Bool
isValidTicket RaffleTicket {ticketSecret, ticketSecretHash} = case ticketSecret of
  Nothing -> False
  Just ts -> sha2_256 ts #== ticketSecretHash
{-# INLINEABLE isValidTicket #-}

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
{-# INLINEABLE isRevealedOf #-}

-- | This function receives two 'RaffleTicket' and returns the first one if 'isRevealedOf' the second one, otherwise it returns the second one.
reavealIfMatched :: RaffleTicket -> RaffleTicket -> RaffleTicket
reavealIfMatched new ticket = if new `isRevealedOf` ticket then new else ticket
{-# INLINEABLE reavealIfMatched #-}

{- | This function receives a ''RaffleTicket', and a '[RaffleTicket]'
, and returns a new list with the corresponding ticket revealed.
-}
reavealSecret :: RaffleTicket -> [RaffleTicket] -> [RaffleTicket]
reavealSecret new tickets = reavealIfMatched new #<$> tickets
{-# INLINEABLE reavealSecret #-}

updateRaffle :: RaffleRedeemer -> RaffleDatum -> RaffleDatum
updateRaffle (Buy pkh commits) datum@RaffleDatum {raffleTickets} =
  let updatedListOfTickets = pfoldr (insertTicket pkh) raffleTickets commits
   in datum {raffleTickets = updatedListOfTickets}
updateRaffle (Reveal newtickets) datum@RaffleDatum {raffleTickets} =
  let updatedListOfTickets = pfoldr reavealSecret raffleTickets newtickets
   in datum {raffleTickets = updatedListOfTickets}
updateRaffle _ datum = trace "invalid redeemer to update datum" datum
{-# INLINEABLE updateRaffle #-}

------------------------

-- **  Raffle Functions

------------------------

-- | This is a function which determines the @revealing interval@ for a given 'RaffleDatum'.
getRaffleRevealingValidRange :: RaffleDatum -> POSIXTimeRange
getRaffleRevealingValidRange RaffleDatum {..} = Interval (lowerBound raffleCommitDeadline) (upperBound raffleRevealDeadline)
{-# INLINEABLE getRaffleRevealingValidRange #-}

getRaffleCloseUnderfundedValidRange :: RaffleDatum -> POSIXTimeRange
getRaffleCloseUnderfundedValidRange RaffleDatum {..} = Interval (lowerBound raffleCommitDeadline) (upperBound (raffleCommitDeadline #+ fromInteger (closingWindow raffleParams)))
{-# INLINEABLE getRaffleCloseUnderfundedValidRange #-}

getRaffleCloseUnrevealedValidRange :: RaffleDatum -> POSIXTimeRange
getRaffleCloseUnrevealedValidRange RaffleDatum {..} = Interval (lowerBound raffleRevealDeadline) (upperBound (raffleRevealDeadline #+ fromInteger (closingWindow raffleParams)))
{-# INLINEABLE getRaffleCloseUnrevealedValidRange #-}

getRaffleAccumulatedValue :: RaffleDatum -> Value
getRaffleAccumulatedValue RaffleDatum {..} = lovelaceValueOf (raffleTicketPrice #* plength raffleTickets)
{-# INLINEABLE getRaffleAccumulatedValue #-}

getRaffleUnrevealedTickets :: RaffleDatum -> [RaffleTicket]
getRaffleUnrevealedTickets RaffleDatum {..} = filter (isNothing . ticketSecret) raffleTickets
{-# INLINEABLE getRaffleUnrevealedTickets #-}

getUnrevealedValue :: RaffleDatum -> Value
getUnrevealedValue raffle =
  let unRevealedTickets = getRaffleUnrevealedTickets raffle
   in lovelaceValueOf (raffleTicketPrice raffle #* plength unRevealedTickets)
{-# INLINEABLE getUnrevealedValue #-}

determineRaffleWinner :: RaffleDatum -> PubKeyHash
determineRaffleWinner RaffleDatum {raffleTickets} =
  let randomSeed = sha2_256 $ pmconcat ((\(Just v) -> v) . ticketSecret #<$> raffleTickets)
      winnderID = modulo (bsToInteger randomSeed) (plength raffleTickets)
      winnerTicket = raffleTickets !! winnderID
   in ticketOwner winnerTicket
{-# INLINEABLE determineRaffleWinner #-}

-- | This ensures the link between state token minting policy and current validator
raffleHasValidStateTokenCurrencySymbol :: RaffleValidatorParams -> RaffleDatum -> Bool
raffleHasValidStateTokenCurrencySymbol (RaffleValidatorParams (MintingPolicyHash csbs)) RaffleDatum {..} =
  csbs #== (unCurrencySymbol . fst . unAssetClass) raffleStateTokenAssetClass
{-# INLINEABLE raffleHasValidStateTokenCurrencySymbol #-}

raffleHasAvailableTickets :: RaffleDatum -> Integer -> Bool
raffleHasAvailableTickets RaffleDatum {raffleParams, raffleTickets} noOfNewTickets =
  "The maximum no. of tickets must not be reached"
    `traceIfFalse` ((plength raffleTickets #+ noOfNewTickets) #<= maxNoOfTickets raffleParams)
{-# INLINEABLE raffleHasAvailableTickets #-}

------------------------

-- **  State checks

------------------------

{- | This is a function to check that a RaffleDatum represents a raffle in a @New State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
      * The commiting deadline should not have been passed.
      * The tickets list must be empty.
      * The ticket price of the raffle must be a positive number.
      * The minimum number of tickets must be a positive number higher than 0.
      * The revealing deadline must be with at least revealing window after the committing deadline.
-}
isInNewState :: RaffleDatum -> POSIXTimeRange -> Bool
isInNewState RaffleDatum {..} txInfoValidRange =
  "The raffle must be in a valid new state"
    `traceIfFalse` pand
      [ "tx valid range is not before commit deadline"
          `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
      , "raffleTickets is not empty"
          `traceIfFalse` pnull raffleTickets
      , "ticket price is negative"
          `traceIfFalse` (raffleTicketPrice #> 0)
      , "invalid min. no. of tickets "
          `traceIfFalse` (raffleMinNoOfTickets #> 0)
      , "min. reveal window not met"
          `traceIfFalse` (getPOSIXTime (raffleRevealDeadline #- raffleCommitDeadline) #> minRevealingWindow raffleParams)
      ]
{-# INLINEABLE isInNewState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in a @Expired State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * The tickets list must be empty.
-}
isInExpiredState :: RaffleDatum -> POSIXTimeRange -> Bool
isInExpiredState RaffleDatum {raffleTickets, raffleCommitDeadline} txInfoValidRange =
  "The raffle must be in a valid expired state"
    `traceIfFalse` pand
      [ "tx valid range is not after commit deadline"
          `traceIfFalse` (raffleCommitDeadline `before` txInfoValidRange)
      , "raffleTickets is not empty"
          `traceIfFalse` pnull raffleTickets
      ]
{-# INLINEABLE isInExpiredState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in a @Committing State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The commiting deadline have not passed.
     * At least one ticket have been bought for the current raffle.
-}
isInCommitState :: RaffleDatum -> POSIXTimeRange -> Bool
isInCommitState RaffleDatum {raffleTickets, raffleCommitDeadline} txInfoValidRange =
  pand
    [ "tx valid range is not before commit deadline"
        `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
    , "raffleTickets must not be empty " `traceIfFalse` (not . pnull) raffleTickets
    ]
{-# INLINEABLE isInCommitState #-}

{- | This is a function to check that a RaffleDatum represents a raffle in an @Underfunded State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * Tickets have been bought.
     * The minimum amount was not reached.
     * The closing deadline (closing window after committing deadline) has not passed.
-}
isInUnderfundedState :: RaffleDatum -> POSIXTimeRange -> Bool
isInUnderfundedState raffle@RaffleDatum {..} txInfoValidRange =
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

{- | This is a function to check that a RaffleDatum represents a raffle in an @Underfunded Exposed State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * Tickets have been bought.
     * The minimum amount was not reached.
     * The closing deadline (closing window after committing deadline) has passed.
-}
isInUnderfundedExposedState :: RaffleDatum -> POSIXTimeRange -> Bool
isInUnderfundedExposedState RaffleDatum {..} txInfoValidRange =
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

{- | This is a function to check that a RaffleDatum represents a raffle in a @Revealing State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The commiting deadline has passed.
     * The minimum number of tickets to be sold was reached. (which implies that tickets have been bought)
     * Not all tickets were revealed yet.
     * The revealing deadline have not been passed.
-}
isInRevealingState :: RaffleDatum -> POSIXTimeRange -> Bool
isInRevealingState datum@RaffleDatum {..} txInfoValidRange =
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

{- | This is a function to check that a RaffleDatum represents a raffle in a @Unrevealed State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The revealing deadline has passed.
     * The minimum number of tickets to be sold was reached. (which implies that tickets have been bought)
     * Not all ticket secrets where revealed.
     * The closing deadline (closing window after revealing deadline) have not passed.
-}
isInUnrevealedState :: RaffleDatum -> POSIXTimeRange -> Bool
isInUnrevealedState raffle@RaffleDatum {..} txInfoValidRange =
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

{- | This is a function to check that a RaffleDatum represents a raffle in a @Unrevealed State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The revealing deadline has passed.
     * The minimum number of tickets to be sold was reached. (which implies that tickets have been bought)
     * Not all ticket secrets where revealed.
     * The closing deadline (closing window after revealing deadline) has passed.
-}
isInUnrevealedExposedState :: RaffleDatum -> POSIXTimeRange -> Bool
isInUnrevealedExposedState raffle@RaffleDatum {..} txInfoValidRange =
  "The raffle must be in a valid unrevealed  exposed state"
    `traceIfFalse` pand
      [ "tx valid range must be after reveal deadline + closing widnow"
          `traceIfFalse` ((raffleRevealDeadline #+ fromInteger (closingWindow raffleParams)) `before` txInfoValidRange)
      , "unrevealed tickets must exists"
          `traceIfFalse` (not . pnull) (getRaffleUnrevealedTickets raffle)
      , "min. no. of tickets must have been sold"
          `traceIfFalse` ((#>= raffleMinNoOfTickets) . plength $ raffleTickets)
      ]

{- | This is a function to check that a RaffleDatum represents a raffle in a @Winner Selected By CRS State@, in a given 'POSIXTimeRange'
The function returns 'True' if the following conditions are met:
     * The commiting deadline should not have been passed.
     * At least one ticket have been bought for the current raffle.
-}
isWinnerSelectedByCRS :: RaffleDatum -> POSIXTimeRange -> Bool
isWinnerSelectedByCRS RaffleDatum {..} txInfoValidRange =
  "The raffle must be in a valid Winner Selected by CRS state"
    `traceIfFalse` pand
      [ "tx valid range must be after revealing deadline" `traceIfFalse` (raffleRevealDeadline `before` txInfoValidRange)
      , "all tickets must be revealed" `traceIfFalse` pall (isJust . ticketSecret) raffleTickets
      ]
{-# INLINEABLE isWinnerSelectedByCRS #-}

------------------------

-- **  Helper functions for raffle in transaction context

------------------------

txIsBurningStateToken :: TxInfo -> RaffleDatum -> Bool
txIsBurningStateToken TxInfo {txInfoMint} RaffleDatum {raffleStateTokenAssetClass} =
  let AssetClass (policyCurrencySymbol, stateTokenName) = raffleStateTokenAssetClass
   in isInValue (policyCurrencySymbol, stateTokenName, -1) txInfoMint
{-# INLINEABLE txIsBurningStateToken #-}

txIsPayingValueTo :: TxInfo -> Value -> PubKeyHash -> Bool
txIsPayingValueTo txInfo value pkh =
  let txOutsToPKH = filter ((#== pubKeyHashAddress pkh) . txOutAddress) (txInfoOutputs txInfo)
      paidValue = psum (txOutValue #<$> txOutsToPKH)
   in paidValue `geq` value
{-# INLINEABLE txIsPayingValueTo #-}

txIsPayingPrizeToPKH :: TxInfo -> RaffleDatum -> PubKeyHash -> Bool
txIsPayingPrizeToPKH txInfo RaffleDatum {..} pkh =
  pand
    [ "The transaction must pay the prize back to the pkh"
        `traceIfFalse` txIsPayingValueTo txInfo rafflePrizeValue pkh
    ]
{-# INLINEABLE txIsPayingPrizeToPKH #-}

txIsPayingPrizeToOrganizer :: TxInfo -> RaffleDatum -> Bool
txIsPayingPrizeToOrganizer txInfo RaffleDatum {..} =
  pand
    [ "The transaction must be signed by the raffle organizer"
        `traceIfFalse` txSignedBy txInfo raffleOrganizer
    , "The transaction must pay the prize back to the raffle organizer"
        `traceIfFalse` txIsPayingValueTo txInfo rafflePrizeValue raffleOrganizer
    ]
{-# INLINEABLE txIsPayingPrizeToOrganizer #-}

txIsPayingPriceToTicketOwner :: TxInfo -> Integer -> RaffleTicket -> Bool
txIsPayingPriceToTicketOwner txInfo price RaffleTicket {..} = txIsPayingValueTo txInfo (lovelaceValueOf price) ticketOwner
{-# INLINEABLE txIsPayingPriceToTicketOwner #-}

txIsRefundingAllTickets :: TxInfo -> RaffleDatum -> Bool
txIsRefundingAllTickets txInfo RaffleDatum {..} =
  "The transaction must pay the ticket price back to each ticket owner."
    `traceIfFalse` pall (txIsPayingPriceToTicketOwner txInfo raffleTicketPrice) raffleTickets
{-# INLINEABLE txIsRefundingAllTickets #-}

txIsPayingAccumulatedValueToOrganizer :: TxInfo -> RaffleDatum -> Bool
txIsPayingAccumulatedValueToOrganizer txInfo raffle@RaffleDatum {..} =
  "The transaction must pay the accumulated value to the raffle organizer."
    `traceIfFalse` txIsPayingValueTo txInfo (getRaffleAccumulatedValue raffle) raffleOrganizer
{-# INLINEABLE txIsPayingAccumulatedValueToOrganizer #-}

txIsRefundingRevealedTickets :: TxInfo -> RaffleDatum -> Bool
txIsRefundingRevealedTickets txInfo RaffleDatum {..} =
  "The transaction must pay the ticket price back to each ticket owner who revealed the ticket secret."
    `traceIfFalse` pall (txIsPayingPriceToTicketOwner txInfo raffleTicketPrice) (filter (isJust . ticketSecret) raffleTickets)
{-# INLINEABLE txIsRefundingRevealedTickets #-}

-- | This is a function which checks if a given transaction is signed by each ticket owner.
txIsSignedByTicketOwners :: ScriptContext -> [RaffleTicket] -> Bool
txIsSignedByTicketOwners ScriptContext {scriptContextTxInfo} tickets =
  "The transaction must be signed by each ticket owner"
    `traceIfFalse` pall ((scriptContextTxInfo `txSignedBy`) . ticketOwner) tickets
{-# INLINEABLE txIsSignedByTicketOwners #-}

txIsPayingUnrevealedValueToPKH :: TxInfo -> RaffleDatum -> PubKeyHash -> Bool
txIsPayingUnrevealedValueToPKH txInfo datum pkh =
  "The transaction must pay the amount for the unrevealed tickets to the pkh"
    `traceIfFalse` txIsPayingValueTo txInfo (getUnrevealedValue datum) pkh
{-# INLINEABLE txIsPayingUnrevealedValueToPKH #-}

------------------------

-- **  Helper functions for raffle in script context

------------------------

ctxGetExtraAmount :: ScriptContext -> RaffleDatum -> Value
ctxGetExtraAmount context raffle@RaffleDatum {..} =
  getOwnInputValue context
    #- ( getRaffleAccumulatedValue raffle
          #+ rafflePrizeValue
          #+ assetClassValue raffleStateTokenAssetClass 1
       )
{-# INLINEABLE ctxGetExtraAmount #-}

ctxIsPayingDonation :: ScriptContext -> RaffleDatum -> Bool
ctxIsPayingDonation context@ScriptContext {scriptContextTxInfo} raffle@RaffleDatum {..} = txIsPayingValueTo scriptContextTxInfo (ctxGetExtraAmount context raffle) (donationPKH raffleParams)
{-# INLINEABLE ctxIsPayingDonation #-}

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
{-# INLINEABLE ctxIsBurningStateTokenAndPayingAnyExtraToDonation #-}

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

------------------------

-- *  Samples