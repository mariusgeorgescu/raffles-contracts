-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.RaffleStateThreadNFT where

import Contracts.Raffle (RaffleDatum (..), RaffleParams (..), getRafflePrizeValue, hasGivenInlineDatum, isInNewState, isInValue, isValidRaffle, outHas1of)
import Contracts.Raffle qualified as Raffle
import Contracts.Samples.NFT qualified as NFT
import Jambhala.Plutus
import Jambhala.Utils
import Ledger.Tx.Constraints.TxConstraints (mustPayToOtherScriptWithInlineDatum)
import Plutus.Script.Utils.Value (geq)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime))

-- | Custom redeemer type to indicate minting mode.
data Mode
  = Minting
      { raffle :: RaffleDatum
      , raffleValidatorHash :: ValidatorHash
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

-- | Helper function to check that a UTxO is being spent in the transaction.
hasUtxo :: TxOutRef -> [TxInInfo] -> Bool
hasUtxo oref = pany (\(TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasUtxo #-}

tokenNameFromAssetClass :: AssetClass -> TokenName
tokenNameFromAssetClass (AssetClass (CurrencySymbol csbs, TokenName tnbs)) = TokenName (sha2_256 (csbs `appendByteString` tnbs))
{-# INLINEABLE tokenNameFromAssetClass #-}

hasCreateRaffleOutput :: ScriptContext -> RaffleDatum -> TxOut -> Bool
hasCreateRaffleOutput context raffle out =
  let policyCurrencySymbol = ownCurrencySymbol context
      stateTokenName = tokenNameFromAssetClass (raffleTokenAssetClass raffle)
   in pand
        [ out `hasGivenInlineDatum` raffle
        , out `outHas1of` raffleTokenAssetClass raffle
        , out `outHas1of` AssetClass (policyCurrencySymbol, stateTokenName)
        ]
{-# INLINEABLE hasCreateRaffleOutput #-}

-- | Typed parameterized minting policy lambda.
nftLambda :: RaffleParams -> Mode -> ScriptContext -> Bool
nftLambda params mode context@(ScriptContext txInfo@TxInfo {..} _) =
  let policyCurrencySymbol = ownCurrencySymbol context
   in case mode of
        Minting raffle@RaffleDatum {} vh ->
          let stateTokenName = tokenNameFromAssetClass (raffleTokenAssetClass raffle)
              txOutsToVH = filter ((#== scriptHashAddress vh) . txOutAddress) txInfoOutputs
           in pand
                [ "The raffle parameters must be in datum"
                    `traceIfFalse` (params #== raffleParams raffle)
                , "The raffle parameters must be valid"
                    `traceIfFalse` isValidRaffle raffle
                , "The raffle datum must be in a valid new state"
                    `traceIfFalse` isInNewState raffle txInfo
                , "The transaction must mint a token with token name equal to raffle's nft assetclass"
                    `traceIfFalse` isInValue (policyCurrencySymbol, stateTokenName, 1) txInfoMint
                , "The transaction must have an output (with the nft , state token name and datum) locked at the raffle validator's address"
                    `traceIfFalse` pany (hasCreateRaffleOutput context raffle) txOutsToVH
                , "The transaction must be signed by the raffle's organizer"
                    `traceIfFalse` txSignedBy txInfo (raffleOrganizer raffle)
                ]
        Burning raffle ->
          let stateTokenName = tokenNameFromAssetClass (raffleTokenAssetClass raffle)
           in "The transaction must burn a token with token name equal to raffle's nft assetclass"
                `traceIfFalse` isInValue (policyCurrencySymbol, stateTokenName, -1) txInfoMint
{-# INLINEABLE nftLambda #-}

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
    (defExports policy)
      { -- Export JSON representations of our redeemer Mode values for transaction construction.
        dataExports = [Minting sampleRaffleNew raffleVal `toJSONfile` "rafflemintmode", Burning sampleRaffleNew `toJSONfile` "raffleburnmode"]
      , emulatorTest = test
      }
  where
    policy = compileScript sampleRafflePrams
    raffleVal = validatorHash (unValidatorContract Raffle.compileValidator)

sampleRafflePrams :: RaffleParams
sampleRafflePrams =
  RaffleParams
    { closingWindow = 1800000 -- Miliseconds
    , minRevealingWindow = 1800000 -- Miliseconds
    , donationPKH = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
    , maxNoOfTickets = 10
    }

sampleRaffleNew :: RaffleDatum
sampleRaffleNew =
  RaffleDatum
    { raffleOrganizer = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
    , raffleTokenAssetClass = nftAssetClass
    , raffleTicketPrice = 100
    , raffleMinNoOfTickets = 5
    , raffleCommitDeadline = POSIXTime 1734961232000
    , raffleRevealDeadline = POSIXTime 1734964832000
    , raffleTickets = []
    , raffleParams = sampleRafflePrams
    }

-- To produce the finished minting policy, select an arbitrary UTxO at your address to consume during the mint.
-- Apply unsafeMkTxOutRef to its TxHash and TxIx values to construct a TxOutRef value, and provide this as argument to the parameterized policy.
-- This UTxO must be included as an input to your minting transaction.
-- for emulator use: "899b40a640d4d3df5bb4a85b0d03be7df0509bcd7f6c1e99075423852a35a2a4" 10
nftCurrencySymbol :: CurrencySymbol
nftCurrencySymbol = getCurrencySymbol $ NFT.compileScript (NFT.PolicyParam (unsafeMkTxOutRef "019b759d7d22f8f93125c27229debf4771194f9d9776acd31e3b0ac4bda04c9a" 0) "jambtoken")

nftAssetClass :: AssetClass
nftAssetClass = AssetClass (nftCurrencySymbol, TokenName "jambtoken")

-- -- 5. Define Emulator Component

instance MintingEndpoint RaffleStateTokenMinting where
  data MintParam RaffleStateTokenMinting
    = Mint RaffleDatum ValidatorHash
    | Burn RaffleDatum
    deriving (Generic, FromJSON, ToJSON)

  mint :: MintParam RaffleStateTokenMinting -> ContractM RaffleStateTokenMinting ()
  mint (Mint raffle vh) = do
    let policy = compileScript sampleRafflePrams
    let tn = tokenNameFromAssetClass (raffleTokenAssetClass raffle)
    let cs = getCurrencySymbol policy
    let statetokenvalue = singleton cs tn 1
    minterUtxos <- ownUtxos
    now <- getCurrentInterval
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy `andUtxos` minterUtxos
        , constraints =
            mconcat
              [ mustValidateInTimeRange (fromPlutusInterval now)
              , mustSign (raffleOrganizer raffle)
              , mustMintWithRedeemer policy (Minting raffle vh) tn 1
              , mustPayToOtherScriptWithInlineDatum vh (mkDatum raffle) (getRafflePrizeValue raffle <> statetokenvalue <> lovelaceValueOf 3000000)
              ]
        }
    logStr $ "Minted 1 " ++ show tn
  mint (Burn raffle) = do
    let policy = compileScript sampleRafflePrams
    let tn = tokenNameFromAssetClass (raffleTokenAssetClass raffle)
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy
        , constraints = mustMintWithRedeemer policy (Burning raffle) tn (-1)
        }
    logStr $ "Burned 1 " ++ show tn

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @NFT.NFTMinting 1 [NFT.Mint "jambtoken" `forWallet` 1] -- mint NFT to wallet 1
    <> initEmulator @RaffleStateTokenMinting
      1
      [ Mint sampleRaffleNew "812fc438d71cb1c1ed0803eb113dc0c1539914d5d435a7600362d7ae" `forWallet` 1 -- Create raffle to validatorHash
      ]
