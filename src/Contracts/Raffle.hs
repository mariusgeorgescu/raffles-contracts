module Contracts.Raffle where

import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V2.Ledger.Api qualified as P (Map)

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
  , raffleToken :: AssetClass
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

-- 2. Define Helper Functions & Lambda

raffleLamba :: RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
raffleLamba _ _ _ = False

-- Buy :
    -- Tx must contain a valid state token input (CnGP Token) -- raffleToken (must be in valid state)
    -- owner must pay the pirce * noOfTickets to the script
    --  Tx must lock (in one or more outputs) at least the price to script.
    -- Tx must have an output locked at the script address (specified in datum) containing:
        --  (1) state token (CnGP Token)
        --  (2) updated datum  (tickets of the owner, state)

-- | Untyped version of the spending validator lambda.
untypedLambda :: UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator raffleLamba
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type RaffleValidator = ValidatorContract "raffle"

-- | Function for producing the compiled spending validator script.
compileValidator :: RaffleValidator
compileValidator = mkValidatorContract $$(compile [||untypedLambda||])