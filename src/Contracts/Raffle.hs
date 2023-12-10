module Contracts.Raffle where

import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V2.Ledger.Api qualified as P (Map)

-- 1. Declare Types
--  Define any required custom data types
data RaffleStatus
  = NEW
  | COMMITTING
  | REVEALING
  | EXPIRED
  | INVALID
  deriving (Generic, FromJSON, ToJSON, Enum)

data RaffleDatum = RaffleDatum
  { raffleOrganizer :: PubKeyHash
  , raffleValue :: Value
  , raffleStatus :: RaffleStatus
  , raffleTicketPrice :: Value
  , raffleMinNoOfTickets :: Integer
  , raffleCommitDeadline :: POSIXTime
  , raffleRevealDeadline :: POSIXTime
  , raffleTickets :: P.Map BuiltinByteString BuiltinByteString
  }
  deriving (Generic, FromJSON, ToJSON)

data RaffleRedeemer
  = Buy [BuiltinByteString]
  | Reveal [(BuiltinByteString, BuiltinByteString)]
  | Redeem
  | Cancel
  deriving (Generic, FromJSON, ToJSON)

-- Generating ToData/FromData instances for the above types via Template Haskell
unstableMakeIsData ''RaffleStatus
makeIsDataIndexed ''RaffleDatum [('RaffleDatum, 0)]
makeIsDataIndexed ''RaffleRedeemer [('Buy, 0), ('Reveal, 1), ('Redeem, 2), ('Cancel, 3)]

-- 2. Define Helper Functions & Lambda

raffleLamba :: RaffleDatum -> RaffleRedeemer -> ScriptContext -> Bool
raffleLamba _ _ _ = False

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