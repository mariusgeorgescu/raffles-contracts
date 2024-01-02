module RafflesDApp.OffChain.Operations where

import Cardano.Api (calculateMinimumUTxO, evaluateTransactionFee)
import Cardano.Api.Shelley (calcMinimumDeposit)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (Except)
import Data.Either.Extra (eitherToMaybe)
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import Jambhala.Plutus (from)
import Jambhala.Plutus qualified
import Jambhala.Utils qualified
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
  -- | Raffle minting policy params
  RaffleParams ->
  -- | Raffle validator params
  RaffleValidatorParams ->
  -- | Raffle to be created.
  RaffleDatum ->
  -- | Raffle Validator Hash of a raffle validator parameterized with the minting policy
  Jambhala.Plutus.ValidatorHash ->
  m (GYTxSkeleton 'PlutusV2)
createRaffle mpParams validatorParams raffle vh = do
  let mp = compileRafflesStateTokenMintingPolicyGY mpParams
  let validator = compileRafflesValidatorGY validatorParams

  oref <- txOutRefToPlutus <$> someUTxO
  tn <- tokenNameFromPlutus' $ tokenNameFromTxOutRef oref
  now <- currentSlot
  currentTime <- slotToBeginTime now
  raffleValidatorAddress <- scriptAddress validator
  prizeVal <- valueFromPlutus' (rafflePrizeValue raffle)
  stateTokenAssetClass <- assetClassFromPlutus' (raffleStateTokenAssetClass raffle)
  if isInNewState raffle (from (timeToPlutus currentTime))
    then
      return $
        mconcat
          [ isInvalidBefore now
          , mustMint mp (redeemerFromPlutusData $ RaffleStateThreadNFT.Minting raffle vh oref) tn 1
          , mustHaveOutput
              GYTxOut
                { gyTxOutAddress = raffleValidatorAddress
                , gyTxOutDatum = Just (datumFromPlutusData raffle, GYTxOutUseInlineDatum)
                , gyTxOutValue = prizeVal <> valueSingleton stateTokenAssetClass 1 <> valueFromLovelace 2_500_000
                , gyTxOutRefS = Nothing
                }
          ]
    else throwAppError InvalidRaffleDatum
