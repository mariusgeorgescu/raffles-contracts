module Contracts.Raffle where

import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V1.Ledger.Interval (after)
import Plutus.V2.Ledger.Api qualified as P (Address, Map)

-- 1. Declare Types
--  Define any required custom data types

data RaffleTicket = RaffleTicket
  { owner :: PubKeyHash
  , secretHash :: BuiltinByteString
  , secret :: Maybe BuiltinByteString
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleDatum = RaffleDatum
  { raffleOrganizer :: PubKeyHash
  , raffleTokenAssetClass :: AssetClass
  , raffleTicketPrice :: Value
  , raffleMinNoOfTickets :: Integer
  , raffleCommitDeadline :: POSIXTime
  , raffleRevealDeadline :: POSIXTime
  , raffleTickets :: P.Map Integer RaffleTicket
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleRedeemer
  = Buy PubKeyHash [BuiltinByteString]
  | Reveal [(Integer, BuiltinByteString)]
  | Redeem
  | Cancel
  deriving (Generic, FromJSON, ToJSON)

-- Generating ToData/FromData instances for the above types via Template Haskell
unstableMakeIsData ''RaffleTicket
makeIsDataIndexed ''RaffleDatum [('RaffleDatum, 0)]
makeIsDataIndexed ''RaffleRedeemer [('Buy, 0), ('Reveal, 1), ('Redeem, 2), ('Cancel, 3)]

--  Define script parameters type

data RaffleParams = RaffleParams
  { closingWindow :: Interval POSIXTime
  , donationAddress :: P.Address
  }

--  Generate Lift instance
makeLift ''RaffleParams

-- 2. Define Helper Functions & Lambda

raffleLamba :: RaffleParams -> RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
raffleLamba _ _ _ _ = False

-- Buy :
-- Tx must contain a valid state token input (CnGP Token) -- raffleToken (must be in valid state)
-- owner must pay the pirce * noOfTickets to the script
--  Tx must lock (in one or more outputs) at least the price to script.
-- Tx must have an output locked at the script address (specified in datum) containing:
--  (1) state token (CnGP Token)
--  (2) updated datum  (tickets of the owner, state)

noTicketsSold :: RaffleDatum -> Bool
noTicketsSold RaffleDatum {raffleTickets} = pnull raffleTickets

isValidRaffle :: RaffleDatum -> Bool
isValidRaffle _ = False

isInNewState :: RaffleDatum -> TxInfo -> Bool
isInNewState RaffleDatum {raffleCommitDeadline, raffleTickets} TxInfo {txInfoValidRange} =
  pand
    [ not (pnull raffleTickets)
    , raffleCommitDeadline `after` txInfoValidRange
    ]

isInCommittingState :: RaffleDatum -> TxInfo -> Bool
isInCommittingState RaffleDatum {raffleCommitDeadline, raffleTickets} txInfo = pnull raffleTickets && (raffleCommitDeadline `after` txInfoValidRange txInfo)

isInRevealingState :: RaffleDatum -> TxInfo -> Bool
isInRevealingState RaffleDatum {raffleCommitDeadline} txInfo = False

-- | Helper function to check that the correct quantity of the given token is in a Value
isInValue :: (CurrencySymbol, TokenName, Integer) -> Value -> Bool
isInValue (cs, tn, q) = pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q #>= q') . flattenValue

-- | Helper function to check if a 'TxOut' contains exactly 1 quantity of an AssetClass
outHas1of :: TxOut -> AssetClass -> Bool
outHas1of (TxOut _ value _ _) (AssetClass (cs, tn)) = isInValue (cs, tn, 1) value

-- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
inHas1of :: TxInInfo -> AssetClass -> Bool
inHas1of = outHas1of . txInInfoResolved

-- | This function checks if exactly 1 quantity of a given AssetClass is in the list of inputs.
nftIsInInputs :: AssetClass -> [TxInInfo] -> Bool
nftIsInInputs ac = pany (`inHas1of` ac)

-- | Untyped version of the spending validator lambda.
untypedLambda :: RaffleParams -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator . raffleLamba

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type RaffleValidator = ValidatorContract "raffle"

-- | Function for producing the compiled spending validator script.
compileValidator :: RaffleParams -> RaffleValidator
compileValidator raffleParams = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode raffleParams)