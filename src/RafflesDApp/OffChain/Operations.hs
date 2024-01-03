module RafflesDApp.OffChain.Operations where

import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import Jambhala.Plutus qualified
import Jambhala.Utils qualified

import Jambhala.Plutus (scriptCurrencySymbol)
import RafflesDApp.OnChain.RaffleStateThreadNFTMintingPolicy qualified as RaffleStateThreadNFT
import RafflesDApp.OnChain.RaffleValidator

compileRaffleValidatorPlutus :: RaffleValidatorParams -> Jambhala.Plutus.Validator
compileRaffleValidatorPlutus = Jambhala.Utils.unValidatorContract . compileValidator

compileRafflesStateTokenMintingPolicyPlutus :: RaffleParams -> Jambhala.Plutus.MintingPolicy
compileRafflesStateTokenMintingPolicyPlutus = Jambhala.Utils.unMintingContract . RaffleStateThreadNFT.compileScript

data RaffleOperationException = InvalidRaffleDatum | Error String
  deriving (Show)

instance Exception RaffleOperationException
instance IsGYApiError RaffleOperationException

pPOSIXTimeFromSlotInteger :: GYTxQueryMonad m => Integer -> m Jambhala.Plutus.POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . slotFromApi . fromInteger

gySlotFromPOSIXTime :: GYTxQueryMonad m => Jambhala.Plutus.POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)

------------------------

-- *  Create Raffle Transaction

------------------------
createRaffle ::
  (HasCallStack, GYTxMonad m, GYTxQueryMonad m) =>
  -- | Raffle minting policy params
  RaffleParams ->
  -- |  rafflePrizeValue
  Jambhala.Plutus.Value ->
  -- |   raffleTicketPrice
  Integer ->
  -- |    raffleMinNoOfTickets
  Integer ->
  -- raffleCommitDeadline
  Jambhala.Plutus.POSIXTime ->
  -- raffleRevealDeadline
  Jambhala.Plutus.POSIXTime ->
  -- | Own address
  GYAddress ->
  m (GYTxSkeleton 'PlutusV2)
createRaffle paramsMP rPrize rTicketPrice rMinNoOfTickets rCommitDdl rRevealDddl ownAddr = do
  let pRaffleMintingPolicy = compileRafflesStateTokenMintingPolicyPlutus paramsMP
  let pRaffleMintingPolicyHash = Jambhala.Plutus.mintingPolicyHash pRaffleMintingPolicy
  let gyRaffleMintignPolicy = mintingPolicyFromPlutus @ 'PlutusV2 pRaffleMintingPolicy
  let pRaffleValidator = compileRaffleValidatorPlutus (RaffleValidatorParams pRaffleMintingPolicyHash)
  let pRaffleValidatorHash = Jambhala.Plutus.validatorHash pRaffleValidator
  let gyRaffleValidator = validatorFromPlutus @ 'PlutusV2 pRaffleValidator
  gyOrganizerPKH <- addressToPubKeyHash' ownAddr
  let pOrganizerPKH = pubKeyHashToPlutus gyOrganizerPKH
  oref <- txOutRefToPlutus <$> someUTxO
  let stateTokenAssetClass = Jambhala.Plutus.AssetClass (scriptCurrencySymbol pRaffleMintingPolicy, tokenNameFromTxOutRef oref)
  let newRaffle =
        RaffleDatum
          { raffleParams = paramsMP
          , raffleStateTokenAssetClass = stateTokenAssetClass
          , raffleOrganizer = pOrganizerPKH
          , rafflePrizeValue = rPrize
          , raffleTicketPrice = rTicketPrice
          , raffleMinNoOfTickets = rMinNoOfTickets
          , raffleCommitDeadline = rCommitDdl
          , raffleRevealDeadline = rRevealDddl
          , raffleTickets = []
          }
  now <- currentSlot
  after36h <- advanceSlot' now 129600
  commitDdllSlot <- gySlotFromPOSIXTime rCommitDdl
  gyTokenName <- tokenNameFromPlutus' $ tokenNameFromTxOutRef oref
  raffleValidatorAddress <- scriptAddress gyRaffleValidator
  gyPrizeVal <- valueFromPlutus' rPrize
  gyStateTokenAssetClass <- assetClassFromPlutus' stateTokenAssetClass
  return $
    mconcat
      [ isInvalidBefore now
      , isInvalidAfter (minimum [after36h, commitDdllSlot])
      , mustMint gyRaffleMintignPolicy (redeemerFromPlutusData $ RaffleStateThreadNFT.Minting newRaffle pRaffleValidatorHash oref) gyTokenName 1
      , mustHaveOutput
          GYTxOut
            { gyTxOutAddress = raffleValidatorAddress
            , gyTxOutDatum = Just (datumFromPlutusData newRaffle, GYTxOutUseInlineDatum)
            , gyTxOutValue = gyPrizeVal <> valueSingleton gyStateTokenAssetClass 1
            , gyTxOutRefS = Nothing
            }
      , mustBeSignedBy gyOrganizerPKH
      ]
