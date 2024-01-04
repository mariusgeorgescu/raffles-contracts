-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}

module RafflesDApp.OnChain.RaffleStateThreadNFTMintingPolicy where

import Contracts.Samples.NFT qualified as NFT
import Jambhala.Plutus
import Jambhala.Utils
import Ledger.Tx.Constraints.TxConstraints (mustPayToOtherScriptWithInlineDatum)
import Plutus.V1.Ledger.Value (geq)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime))
import RafflesDApp.OnChain.RaffleValidator (RaffleDatum (..), RaffleParams (..), RaffleValidatorParams (RaffleValidatorParams), hasGivenInlineDatum, hasUtxo, isInNewState, isInValue, tokenNameFromTxOutRef)
import RafflesDApp.OnChain.RaffleValidator qualified as RaffleValidator

-- | Custom redeemer type to indicate minting mode.
data Mode
  = Minting
      { raffle :: RaffleDatum
      , raffleValidatorHash :: ValidatorHash
      , utxoRefSeed :: TxOutRef
      }
  | Burning
      { raffle :: RaffleDatum
      }
  deriving (Generic, FromJSON, ToJSON)

{- | For custom types with multiple constructors, `makeIsDataIndexed` must be used to generate ToData/FromData instances.
  Unlike `unstableMakeIsData`, this generates `BuiltinData` values with constructors indexed in a stable order.
-}
makeIsDataIndexed ''Mode [('Minting, 0), ('Burning, 1)]

-- 2. Define Lambda

{- | Typed parameterized minting policy lambda for minting and burning Raffle State Thread NFTs.
For minting, the following conditions must be met:
    * The raffle parameters of the minting policy  must be equal with the ones in the datum of the output locked at raffle validator's address.
    * The raffle datum must be a valid datum to create a new raffle.
    * The transaction must mint exactly 1 state token (with token name equal to the seed 'TxOutRef').
    * The transaction must have an output with value containing the raffle's prize value + raffle state thread NFT and a valid datum, locked at the raffle validator's address.
    * The transaction must be signed by the raffle's organizer.
For buring, the transaction must burn exactly 1 token with raffle state token AssetClass.
-}
nftLambda :: RaffleParams -> Mode -> ScriptContext -> Bool
nftLambda params mode context@(ScriptContext txInfo@TxInfo {..} _) =
  case mode of
    Minting raffle@RaffleDatum {} vh txOutRef ->
      let stateTokenName = tokenNameFromTxOutRef txOutRef
          txOutsToVH = filter ((#== scriptHashAddress vh) . txOutAddress) txInfoOutputs
       in pand
            [ "The raffle parameters of the minting policy  must be equal with the ones in the datum of the output locked at raffle validator's address."
                `traceIfFalse` (params #== raffleParams raffle)
            , "The raffle datum must be a valid datum to create a new raffle."
                `traceIfFalse` checkCreateNewRaffle context txOutRef raffle
            , "The transaction must mint exactly 1 state token (with token name equal to the seed 'TxOutRef')."
                `traceIfFalse` isInValue (ownCurrencySymbol context, stateTokenName, 1) txInfoMint
            , "The transaction must have an output (with the raffle state token, rafflePrizeValue and datum) locked at the raffle validator's address."
                `traceIfFalse` pany (`hasPrizeAndStateValueWithDatum` raffle) txOutsToVH
            , "The transaction must be signed by the raffle's organizer."
                `traceIfFalse` txSignedBy txInfo (raffleOrganizer raffle)
            ]
    Burning raffle ->
      let AssetClass (policyCurrencySymbol, stateTokenName) = raffleStateTokenAssetClass raffle
       in "The transaction must burn exactly 1 token with raffle state token AssetClass."
            `traceIfFalse` isInValue (policyCurrencySymbol, stateTokenName, -1) txInfoMint
{-# INLINEABLE nftLambda #-}

{- | This is a function to check that a RaffleDatum is valid for creating a new raffle.
For the datum to be valid, the following conditions must be met:
     * The state token currency symbol must be of the current minting policy. and the state token name must be derived from the seed 'TxOutRef'.
     * The seed 'TxOutRef' must be spent.
     * The raffle must be in a valid new state:
        * The ticket price of the raffle must be a positive number higher than 0..
        * The minimum number of tickets must be a positive number higher than 0.
        * No tickets should have been bought yet for the current raffle.
        * The revealing deadline must be with at least revealing window after the committing deadline.
        * The commiting deadline should not have been passed.
-}
checkCreateNewRaffle :: ScriptContext -> TxOutRef -> RaffleDatum -> Bool
checkCreateNewRaffle context seedTxOutRef raffle@RaffleDatum {..} =
  pand
    [ "invalid state token"
        `traceIfFalse` (AssetClass (ownCurrencySymbol context, tokenNameFromTxOutRef seedTxOutRef) #== raffleStateTokenAssetClass)
    , "seed UTXO not spent"
        `traceIfFalse` hasUtxo seedTxOutRef ((txInfoInputs . scriptContextTxInfo) context)
    , "raffle not in new state"
        `traceIfFalse` isInNewState raffle (txInfoValidRange . scriptContextTxInfo $ context)
    ]
{-# INLINEABLE checkCreateNewRaffle #-}

{- | This function receives a  t'TxOut' and a t'RaffleDatum' and returns v'True' if
  * the t'TxOut' contains the t'RaffleDatum' inlined
  * the t'TxOut' contains a t'Value' with:
     * exactly 1 quantity of the t'AssetClass' specified as 'raffleTokenAssetClass'.
     * the 'Value' specified in 'rafflePrizeValue'.
-}
hasPrizeAndStateValueWithDatum :: TxOut -> RaffleDatum -> Bool
hasPrizeAndStateValueWithDatum out raffle@RaffleDatum {..} =
  let value =
        ( assetClassValue raffleStateTokenAssetClass 1
            #+ rafflePrizeValue
        )
   in pand
        [ txOutValue out `geq` value
        , out `hasGivenInlineDatum` raffle
        ]
{-# INLINEABLE hasPrizeAndStateValueWithDatum #-}

-- -- | Untyped version of the parameterized minting policy lambda.
untypedLambda :: RaffleParams -> UntypedMintingPolicy
untypedLambda p = mkUntypedMintingPolicy $ nftLambda p
{-# INLINEABLE untypedLambda #-}

-- | Declare contract synonym with unique symbolic identifier.
type RaffleStateTokenMinting = MintingContract "raffle-nft"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileScript :: RaffleParams -> RaffleStateTokenMinting
compileScript p =
  mkMintingContract $
    $$(compile [||untypedLambda||])
      `applyCode` liftCode p

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `jamb` CLI.
exports :: JambExports
exports =
  export
    (defExports samplePolicy)
      { -- Export JSON representations of our redeemer Mode values for transaction construction.
        dataExports = [Minting sampleRaffleNew sampleRaffleValidatorHash sampleTxOutRefSeed `toJSONfile` "rafflemintmode", Burning sampleRaffleNew `toJSONfile` "raffleburnmode"]
      , emulatorTest = test
      }

-- To produce the finished minting policy, select an arbitrary UTxO at your address to consume during the mint.
-- Apply unsafeMkTxOutRef to its TxHash and TxIx values to construct a TxOutRef value, and provide this as argument to the parameterized policy.
-- This UTxO must be included as an input to your minting transaction.
-- for emulator use: "899b40a640d4d3df5bb4a85b0d03be7df0509bcd7f6c1e99075423852a35a2a4" 10
nftCurrencySymbol :: CurrencySymbol
nftCurrencySymbol = getCurrencySymbol $ NFT.compileScript (NFT.PolicyParam (unsafeMkTxOutRef "019b759d7d22f8f93125c27229debf4771194f9d9776acd31e3b0ac4bda04c9a" 0) "jambtoken")

nftTokenName :: TokenName
nftTokenName = TokenName "jambtoken"

sampleNFTAssetClass :: AssetClass
sampleNFTAssetClass = AssetClass (nftCurrencySymbol, nftTokenName)

sampleTxOutRefSeed :: TxOutRef
sampleTxOutRefSeed = unsafeMkTxOutRef "019b759d7d22f8f93125c27229debf4771194f9d9776acd31e3b0ac4bda04c9a" 1

sampleRafflePrams :: RaffleParams
sampleRafflePrams =
  RaffleParams
    { closingWindow = 1000 -- Miliseconds
    , minRevealingWindow = 1000 -- Miliseconds
    , donationPKH = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
    , maxNoOfTickets = 10
    }

samplePolicy :: RaffleStateTokenMinting
samplePolicy = compileScript sampleRafflePrams
samplePolicyHash :: MintingPolicyHash
samplePolicyHash = mintingPolicyHash (unMintingContract samplePolicy)
sampleRaffleValidatorParams :: RaffleValidatorParams
sampleRaffleValidatorParams = RaffleValidatorParams samplePolicyHash
sampleCompileRaffleValidator :: Validator
sampleCompileRaffleValidator = unValidatorContract (RaffleValidator.compileValidator sampleRaffleValidatorParams)
sampleRaffleValidatorHash :: ValidatorHash
sampleRaffleValidatorHash = validatorHash sampleCompileRaffleValidator

sampleRaffleNew :: RaffleDatum
sampleRaffleNew =
  RaffleDatum
    { raffleOrganizer = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
    , rafflePrizeValue = singleton nftCurrencySymbol nftTokenName 1
    , raffleTicketPrice = 100
    , raffleMinNoOfTickets = 5
    , raffleCommitDeadline = POSIXTime 1734964832000
    , raffleRevealDeadline = POSIXTime 1744964832000
    , raffleTickets = []
    , raffleParams = sampleRafflePrams
    , raffleStateTokenAssetClass = AssetClass (getCurrencySymbol samplePolicy, tokenNameFromTxOutRef sampleTxOutRefSeed)
    }

-- -- 5. Define Emulator Component

instance MintingEndpoint RaffleStateTokenMinting where
  data MintParam RaffleStateTokenMinting
    = Mint RaffleDatum ValidatorHash
    | Burn RaffleDatum
    deriving (Generic, FromJSON, ToJSON)

  mint :: MintParam RaffleStateTokenMinting -> ContractM RaffleStateTokenMinting ()
  mint (Mint raffle vh) = do
    let policy = compileScript sampleRafflePrams
    let cs = getCurrencySymbol policy
    oref <- getUnspentOutput
    let tn = tokenNameFromTxOutRef oref
    let statetokenvalue = singleton cs tn 1
    logStr $ "TxOutRef Seed:" ++ show oref
    minterUtxos <- ownUtxos
    now <- getCurrentInterval
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy `andUtxos` minterUtxos
        , constraints =
            mconcat
              [ mustValidateInTimeRange (fromPlutusInterval now)
              , mustSign (raffleOrganizer raffle)
              , mustMintWithRedeemer policy (Minting raffle vh oref) tn 1
              , mustPayToOtherScriptWithInlineDatum vh (mkDatum raffle) (rafflePrizeValue raffle <> statetokenvalue <> lovelaceValueOf 2500000)
              ]
        }
    logStr $ "Minted 1 " ++ show tn
  mint (Burn raffle) = do
    let policy = compileScript sampleRafflePrams
    let AssetClass (_, stateTokenName) = raffleStateTokenAssetClass raffle
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy
        , constraints = mustMintWithRedeemer policy (Burning raffle) stateTokenName (-1)
        }
    logStr $ "Burned 1 " ++ show stateTokenName

-- | Define emulator test.
test :: EmulatorTest
test =
  mconcat
    [ initEmulator @NFT.NFTMinting 1 [NFT.Mint "jambtoken" `forWallet` 1] -- mint NFT to wallet 1
    , initEmulator @RaffleStateTokenMinting
        1
        [ Mint sampleRaffleNew sampleRaffleValidatorHash `forWallet` 1 -- Create raffle to validatorHash
        ]
    ]
