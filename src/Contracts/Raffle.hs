module Contracts.Raffle where

import Jambhala.Plutus hiding (Address)
import Jambhala.Utils
import Plutus.V1.Ledger.Interval (after)
import Plutus.V1.Ledger.Value (geq)
import Plutus.V2.Ledger.Api

-- 1. Declare Types
--  Define any required custom data types

data RaffleTicket = RaffleTicket
  { ticketOwner :: PubKeyHash
  , ticketNumber :: Integer
  , ticketSecretHash :: BuiltinByteString
  , ticketSecret :: Maybe BuiltinByteString
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleDatum = RaffleDatum
  { raffleOrganizer :: PubKeyHash
  , raffleTokenAssetClass :: AssetClass
  , raffleTicketPrice :: Integer
  , raffleMinNoOfTickets :: Integer
  , raffleCommitDeadline :: POSIXTime
  , raffleRevealDeadline :: POSIXTime
  , raffleTickets :: [RaffleTicket]
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleRedeemer
  = Create
  | Buy PubKeyHash [BuiltinByteString]
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
makeIsDataIndexed ''RaffleDatum [('RaffleDatum, 0)]
makeIsDataIndexed
  ''RaffleRedeemer
  [ ('Create, 0)
  , ('Buy, 1)
  , ('Reveal, 2)
  , ('Redeem, 3)
  , ('Cancel, 4)
  , ('CloseExpired, 5)
  , ('CloseUnderfunded, 6)
  , ('CloseUnrevealed, 7)
  , ('CloseExposedUnderfunded, 8)
  , ('CloseExposedUnrevealed, 9)
  ]

--  Define script parameters type

data RaffleParams = RaffleParams
  { closingWindow :: Integer -- Miliseconds
  , minRevealingWindow :: Integer -- Miliseconds
  , donationAddress :: Address
  , maxNoOfTickets :: Integer
  }

--  Generate Lift instance
makeLift ''RaffleParams

-- 2. Define Helper Functions & Lambda

raffleLamba :: RaffleParams -> RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
raffleLamba params datum redeemer context =
  let txInfo = scriptContextTxInfo context
      raffleouts = getContinuingOutputs context
   in case redeemer of
        Create ->
          pand
            [ "Invalid raffle" `traceIfFalse` isValidRaffle params datum
            , "Invalid new state" `traceIfFalse` isInNewState datum txInfo
            , "No continuing output found" `traceIfFalse` pany (`isTxOutWithNFTandDatum` datum) raffleouts
            , "Not signed by the organizer" `traceIfFalse` (txInfo `txSignedBy` raffleOrganizer datum)
            ]
        Buy _pkh _commits ->
          let updatedDatum = updateRaffle redeemer datum
           in pand
                [ "Commit deadline must not be reached" `traceIfFalse` (raffleCommitDeadline datum `after` txInfoValidRange txInfo)
                , "Maximum no of tickets must not be reached" `traceIfFalse` ((plength . raffleTickets) updatedDatum #<= maxNoOfTickets params) -- Param constraint
                , "The validating input must contain proof token" `traceIfFalse` spendsProof (raffleTokenAssetClass datum) context
                , "Transaction must have exactly 1 outptut with NFT and valid datum" `traceIfFalse` (context `txHas1OutWithNFTandDatum` datum)
                ]
        Reveal revealed_tickets ->
          let updatedDatum = updateRaffle redeemer datum
           in pand
                [ "Invalid revealing state" `traceIfFalse` isInRevealingState datum txInfo
                , "The validating input must contain proof token" `traceIfFalse` spendsProof (raffleTokenAssetClass datum) context
                , "Transaction must have exactly 1 outptut with NFT and valid datum" `traceIfFalse` (context `txHas1OutWithNFTandDatum` datum)
                , "Not signed by ticket owner" `traceIfFalse` (txInfo `isSignedByTicketOwners` revealed_tickets)
                ]
        Redeem -> False
        Cancel ->
          pand
            [ "Invalid new state" `traceIfFalse` isInNewState datum txInfo
            , "The validating input must contain proof token" `traceIfFalse` spendsProof (raffleTokenAssetClass datum) context
            , --- nft to organzier
              --- any value locked at script to donation
              "Not signed by the organizer" `traceIfFalse` (txInfo `txSignedBy` raffleOrganizer datum)
            ]
        CloseExpired -> False
        CloseUnderfunded -> False
        CloseUnrevealed -> False
        CloseExposedUnderfunded pkh -> False
        CloseExposedUnrevealed pkh -> False

------------------CREATE RAFFLE -----------

{- | This function receives a  t'TxOut' and a t'RaffleDatum' and returns v'True' if the following conditions are met:
     * t'TxOut' contains the t'RaffleDatum' inlined.
     * t'TxOut' contains exactly 1 quantity of the t'AssetClass' specified in the t'RaffleDatum'.
-}
isTxOutWithNFTandDatum :: TxOut -> RaffleDatum -> Bool
isTxOutWithNFTandDatum out datum =
  out `hasGivenInlineDatum` datum
    && out `outHas1of` raffleTokenAssetClass datum

{- | This is a function to check that a RaffleDatum is for a valid raffle.
For the raffle to be valid, the following conditions must be met:
     * The ticket price of the raffle must be a positive number.
     * The minimum number of tickets must be a positive number.
     * The revealing deadline must be with at least revealing window after the committing deadline.
-}
isValidRaffle :: RaffleParams -> RaffleDatum -> Bool
isValidRaffle RaffleParams {minRevealingWindow} RaffleDatum {raffleCommitDeadline, raffleRevealDeadline, raffleTicketPrice, raffleMinNoOfTickets} =
  pand
    [ "ticket price is negative" `traceIfFalse` (raffleTicketPrice #> 0)
    , "min. no. of tickets is negative" `traceIfFalse` (raffleMinNoOfTickets #> 0)
    , "min. reveal window not met" `traceIfFalse` (getPOSIXTime (raffleRevealDeadline - raffleCommitDeadline) #> minRevealingWindow) -- Param constraint
    ]

{- | This is a function to check that a RaffleDatum represents a raffle in a @New State@ in a given transaction context.
The function returns 'True' if the following conditions are met:
     * The commiting deadline should not have been passed.
     * No tickets should have been bought yet for the current raffle.
-}
isInNewState :: RaffleDatum -> TxInfo -> Bool
isInNewState RaffleDatum {raffleTickets, raffleCommitDeadline} TxInfo {txInfoValidRange} =
  pand
    [ "tx valid range is not after commit deadline" `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
    , "raffleTickets is not empty" `traceIfFalse` pnull raffleTickets
    ]

-----------------------------------------
------------------BUY TICKETS -----------

-- isInCommittingState :: RaffleDatum -> TxInfo -> Bool
-- isInCommittingState RaffleDatum {raffleCommitDeadline, raffleTickets} TxInfo {txInfoValidRange} =
--   pand
--     [ (not . pnull) raffleTickets
--     , raffleCommitDeadline `after` txInfoValidRange
--     ]

{- | This is a function to check if the transaction has exactly one output which:
     * contains a the RaffleDatum inlined.
     * contains exactly 1 quantity of the AssetClass specified in the RaffleDatum.
-}
txHas1OutWithNFTandDatum :: ScriptContext -> RaffleDatum -> Bool
txHas1OutWithNFTandDatum context datum = case getContinuingOutputs context of
  [out] -> out `isTxOutWithNFTandDatum` datum
  _ -> trace "Tx does not have exactly 1 ouptut" False

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

--- any value locked ad script to donation

getOwnInputValue :: ScriptContext -> Value
getOwnInputValue context =
  let i = findOwnInput context
   in case i of
        Nothing -> error "No continuing output found"
        Just TxInInfo {txInInfoResolved} -> txOutValue txInInfoResolved

-- | Untyped version of the spending validator lambda.
untypedLambda :: RaffleParams -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator . raffleLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type RaffleValidator = ValidatorContract "raffle"

-- | Function for producing the compiled spending validator script.
compileValidator :: RaffleParams -> RaffleValidator
compileValidator raffleParams = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode raffleParams)

--------
--------
--------
--------
--------
---------

-- | Helper function to check that the correct quantity of the given token is in a Value
isInValue :: (CurrencySymbol, TokenName, Integer) -> Value -> Bool
isInValue (cs, tn, q) = pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q #>= q') . flattenValue

-- | Helper function to check if a 'TxOut' contains exactly 1 quantity of an AssetClass
outHas1of :: TxOut -> AssetClass -> Bool
outHas1of (TxOut _ value _ _) (AssetClass (cs, tn)) = isInValue (cs, tn, 1) value

-- | Helper function to check if a 'TxOut' contains a given datum and is inlined.
hasGivenInlineDatum :: ToData a => TxOut -> a -> Bool
hasGivenInlineDatum out datum = case txOutDatum out of
  OutputDatum da -> toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False

-- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
inputHas1of :: TxInInfo -> AssetClass -> Bool
inputHas1of = outHas1of . txInInfoResolved

-- | Helper function: check that the validating input contains proof token
spendsProof :: AssetClass -> ScriptContext -> Bool
spendsProof proofToken sc = case (`inputHas1of` proofToken) #<$> findOwnInput sc of
  Nothing -> trace "Own input not found" False
  Just result -> traceIfFalse "Proof token not spent" result
{-# INLINEABLE spendsProof #-}

a = singleton (CurrencySymbol "dasda") (TokenName "dasda") 2

b = singleton (CurrencySymbol "dasadsda") (TokenName "dasassda") 3

c = singleton (CurrencySymbol "dasda") (TokenName "dasda") 3

d = singleton (CurrencySymbol "x") (TokenName "x") 2

x = a #+ b

y = c #+ d

--- >>> x #- y
-- Value (Map [(6461736164736461,Map [("dasassda",3)]),(6461736461,Map [("dasda",-1)]),(78,Map [("x",-2)])])

--- >>> y #- x
-- Value (Map [(6461736164736461,Map [("dasassda",-3)]),(6461736461,Map [("dasda",1)]),(78,Map [("x",2)])])

--- >>> y `geq` x
-- False

as = geq

-----------
-----------
-- --------