module Contracts.Raffle where

import Jambhala.Plutus hiding (Address)
import Jambhala.Utils
import Plutus.V1.Ledger.Interval (after)
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
  | Reveal [(BuiltinByteString, BuiltinByteString)]
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
makeIsDataIndexed ''RaffleRedeemer [('Create, 0), ('Buy, 1), ('Reveal, 2), ('Redeem, 3), ('Cancel, 4), ('CloseExpired, 5), ('CloseUnderfunded, 6), ('CloseUnrevealed, 7), ('CloseExposedUnderfunded, 8), ('CloseExposedUnrevealed, 9)]

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
raffleLamba params datum Create context = isValidCreateRaffle params datum context
raffleLamba params datum redeemer@(Buy _ _) context = isValidBuy params redeemer datum context
raffleLamba _ _ _ _ = False

------------------CREATE RAFFLE -----------

-- | Helper function to check that the correct quantity of the given token is in a Value
isInValue :: (CurrencySymbol, TokenName, Integer) -> Value -> Bool
isInValue (cs, tn, q) = pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q #>= q') . flattenValue

-- | Helper function to check if a 'TxOut' contains exactly 1 quantity of an AssetClass
outHas1of :: TxOut -> AssetClass -> Bool
outHas1of (TxOut _ value _ _) (AssetClass (cs, tn)) = isInValue (cs, tn, 1) value

isTxOutWithNFTandDatum :: RaffleDatum -> TxOut -> Bool
isTxOutWithNFTandDatum datum out =
  let hasGivenDatum = case txOutDatum out of
        OutputDatum da -> toBuiltinData datum #== getDatum da
        _ -> trace "Datum must exsist and must be inlined" False
   in hasGivenDatum && out `outHas1of` raffleTokenAssetClass datum

isInNewState :: RaffleParams -> RaffleDatum -> TxInfo -> Bool
isInNewState RaffleParams {minRevealingWindow} RaffleDatum {..} TxInfo {txInfoValidRange} =
  pand
    [ "price must be positive" `traceIfFalse` (raffleTicketPrice #> 0)
    , "the minimum no of tickets must be positive" `traceIfFalse` (raffleMinNoOfTickets #> 0)
    , "reveal deadline must be with at least revealingWindow after commit deadline" `traceIfFalse` (getPOSIXTime (raffleRevealDeadline - raffleCommitDeadline) #> minRevealingWindow)
    , "commit deadline must be after transaction valid range" `traceIfFalse` (raffleCommitDeadline `after` txInfoValidRange)
    , "tickets list must be empty" `traceIfFalse` pnull raffleTickets
    ]

isValidCreateRaffle :: RaffleParams -> RaffleDatum -> ScriptContext -> Bool
isValidCreateRaffle params datum sc =
  let txInfo = scriptContextTxInfo sc
      raffleouts = getContinuingOutputs sc
   in pand
        [ "Raffle must be in a valid New state" `traceIfFalse` isInNewState params datum txInfo
        , "Transaction must have an outptut with NFT and valid datum" `traceIfFalse` pany (isTxOutWithNFTandDatum datum) raffleouts
        , "Transaction must be signed by raffle organizer" `traceIfFalse` txSignedBy txInfo (raffleOrganizer datum)
        ]

-----------------------------------------
------------------BUY TICKETS -----------

-- Buy :
-- Tx must contain a valid state token input (CnGP Token) -- raffleToken (must be in valid state)
-- owner must pay the pirce * noOfTickets to the script
--  Tx must lock (in one or more outputs) at least the price to script.
-- Tx must have an output locked at the script address (specified in datum) containing:
--  (1) state token (CnGP Token)
--  (2) updated datum  (tickets of the owner, state)

isValidBuy :: RaffleParams -> RaffleRedeemer -> RaffleDatum -> ScriptContext -> Bool
isValidBuy params redeemer@(Buy _ _) datum sc =
  let txInfo = scriptContextTxInfo sc
      raffleouts = getContinuingOutputs sc
      updatedDatum = updateRaffle redeemer datum
   in pand
        [ "Commit deadline must not be reached" `traceIfFalse` (raffleCommitDeadline datum `after` txInfoValidRange txInfo)
        , "Maximum no of tickets must not be reached" `traceIfFalse` ((plength . raffleTickets) updatedDatum #<= maxNoOfTickets params)
        , "The validating input must contain proof token" `traceIfFalse` spendsProof (raffleTokenAssetClass datum) sc
        , "Transaction must have exactly 1 outptut with NFT and valid datum" `traceIfFalse` (((== 1) . plength) raffleouts && (isTxOutWithNFTandDatum updatedDatum . head) raffleouts)
        ]
isValidBuy _ _ _ _ = False

isInCommittingState :: RaffleDatum -> TxInfo -> Bool
isInCommittingState RaffleDatum {raffleCommitDeadline, raffleTickets} TxInfo {txInfoValidRange} =
  pand
    [ (not . pnull) raffleTickets
    , raffleCommitDeadline `after` txInfoValidRange
    ]

-- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
inHas1of :: TxInInfo -> AssetClass -> Bool
inHas1of = outHas1of . txInInfoResolved

-- | Helper function: check that the validating input contains proof token
spendsProof :: AssetClass -> ScriptContext -> Bool
spendsProof proofToken sc = case (`inHas1of` proofToken) #<$> findOwnInput sc of
  Nothing -> trace "Own input not found" False
  Just result -> traceIfFalse "Proof token not spent" result
{-# INLINEABLE spendsProof #-}

insertTicket :: PubKeyHash -> BuiltinByteString -> [RaffleTicket] -> [RaffleTicket]
insertTicket pkh secrethash tickets =
  let count = plength tickets
   in RaffleTicket pkh count secrethash Nothing : tickets

updateTicket :: BuiltinByteString -> BuiltinByteString -> RaffleTicket -> RaffleTicket
updateTicket secrethash secret ticket = if sha2_256 secret == secrethash then ticket {ticketSecret = Just secret} else ticket

reavealSecret :: (BuiltinByteString, BuiltinByteString) -> [RaffleTicket] -> [RaffleTicket]
reavealSecret (secrethash, secret) tickets = updateTicket secrethash secret <$> tickets

updateRaffle :: RaffleRedeemer -> RaffleDatum -> RaffleDatum
updateRaffle (Buy pkh commits) datum@RaffleDatum {raffleTickets} =
  let updatedListOfTickets = pfoldr (insertTicket pkh) raffleTickets commits
   in datum {raffleTickets = updatedListOfTickets}
updateRaffle (Reveal pairs) datum@RaffleDatum {raffleTickets} =
  let updatedListOfTickets = pfoldr reavealSecret raffleTickets pairs
   in datum {raffleTickets = updatedListOfTickets}
updateRaffle _ _ = error "invalid redeemer to update datum"

-- --------
--------
--------
--------
--------
--------
--------

-- | Untyped version of the spending validator lambda.
untypedLambda :: RaffleParams -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator . raffleLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type RaffleValidator = ValidatorContract "raffle"

-- | Function for producing the compiled spending validator script.
compileValidator :: RaffleParams -> RaffleValidator
compileValidator raffleParams = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode raffleParams)
