module RafflesDApp.OffChain.Operations where

import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types
import Jambhala.Plutus (AssetClass (..), from)
import Jambhala.Plutus qualified
import Jambhala.Utils qualified
import Plutus.Model.Fork.Ledger.TimeSlot (posixTimeToEnclosingSlot)
import RafflesDApp.OnChain.RaffleStateThreadNFTMintingPolicy qualified as RaffleStateThreadNFT
import RafflesDApp.OnChain.RaffleValidator

-- | Validator in question, obtained after giving required parameters.
compileRafflesValidatorGY :: RaffleValidatorParams -> GYValidator 'PlutusV2
compileRafflesValidatorGY = validatorFromPlutus . Jambhala.Utils.unValidatorContract . compileValidator

-- | Validator in question, obtained after giving required parameters.
compileRafflesStateTokenMintingPolicyGY :: RaffleParams -> GYMintingPolicy 'PlutusV2
compileRafflesStateTokenMintingPolicyGY = mintingPolicyFromPlutus . Jambhala.Utils.unMintingContract . RaffleStateThreadNFT.compileScript

data RaffleOperationException = InvalidRaffleDatum | Error String
  deriving (Show)

instance Exception RaffleOperationException
instance IsGYApiError RaffleOperationException

-- | Operation to create a raffle.
createRaffle ::
  (HasCallStack, GYTxMonad m, GYTxQueryMonad m) =>
  -- | OwnAddress
  GYAddress ->
  -- | Raffle minting policy params
  RaffleParams ->
  -- | Raffle validator params
  RaffleValidatorParams ->
  -- | Raffle to be created.
  RaffleDatum ->
  -- | Raffle Validator Hash of a raffle validator parameterized with the minting policy
  Jambhala.Plutus.ValidatorHash ->
  m (GYTxSkeleton 'PlutusV2)
createRaffle ownAddr mpParams validatorParams raffle vh = do
  let mp = compileRafflesStateTokenMintingPolicyGY mpParams
  let validator = compileRafflesValidatorGY validatorParams
  ownpkh <- addressToPubKeyHash' ownAddr
  oref <- txOutRefToPlutus <$> someUTxO
  tn <- tokenNameFromPlutus' $ tokenNameFromTxOutRef oref
  now <- currentSlot
  next4 <- advanceSlot' now 4
  currentTime <- slotToBeginTime now
  raffleValidatorAddress <- scriptAddress validator
  prizeVal <- valueFromPlutus' (rafflePrizeValue raffle)
  let AssetClass (cs, plm) = raffleStateTokenAssetClass raffle
  let stateTokenAssetClass = AssetClass (cs, tokenNameFromTxOutRef oref)
  stateTokenAssetClassGY <- assetClassFromPlutus' stateTokenAssetClass
  let raffle2 = raffle {raffleStateTokenAssetClass = stateTokenAssetClass, raffleOrganizer = pubKeyHashToPlutus ownpkh}
  if True -- isInNewState raffle (from (timeToPlutus currentTime))
    then
      return $
        mconcat
          [ isInvalidBefore now
          , isInvalidAfter next4
          , mustMint mp (redeemerFromPlutusData $ RaffleStateThreadNFT.Minting raffle2 vh oref) tn 1
          , mustHaveOutput
              GYTxOut
                { gyTxOutAddress = raffleValidatorAddress
                , gyTxOutDatum = Just (datumFromPlutusData (raffle2), GYTxOutUseInlineDatum)
                , gyTxOutValue = prizeVal <> valueSingleton (stateTokenAssetClassGY) 1  
                , gyTxOutRefS = Nothing
                }
          , mustBeSignedBy ownpkh
          ]
    else throwAppError InvalidRaffleDatum
