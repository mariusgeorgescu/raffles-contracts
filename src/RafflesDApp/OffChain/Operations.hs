module RafflesDApp.OffChain.Operations where

import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import Jambhala.Plutus qualified as JP
import Jambhala.Utils qualified

import Jambhala.Plutus (UnsafeFromData (unsafeFromBuiltinData))

import RafflesDApp.OnChain.RaffleStateThreadNFTMintingPolicy qualified as RaffleStateThreadNFT
import RafflesDApp.OnChain.RaffleValidator

compileRaffleValidatorPlutus :: RaffleValidatorParams -> JP.Validator
compileRaffleValidatorPlutus = Jambhala.Utils.unValidatorContract . compileValidator

compileRafflesStateTokenMintingPolicyPlutus :: RaffleParams -> JP.MintingPolicy
compileRafflesStateTokenMintingPolicyPlutus = Jambhala.Utils.unMintingContract . RaffleStateThreadNFT.compileScript

data RaffleOperationException = InvalidRaffleDatum | Error String
  deriving (Show)

instance Exception RaffleOperationException
instance IsGYApiError RaffleOperationException

pPOSIXTimeFromSlotInteger :: GYTxQueryMonad m => Integer -> m JP.POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . slotFromApi . fromInteger

gySlotFromPOSIXTime :: GYTxQueryMonad m => JP.POSIXTime -> m GYSlot
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
  JP.Value ->
  -- |   raffleTicketPrice
  Integer ->
  -- |    raffleMinNoOfTickets
  Integer ->
  -- raffleCommitDeadline
  JP.POSIXTime ->
  -- raffleRevealDeadline
  JP.POSIXTime ->
  -- | Own address
  GYAddress ->
  m (GYTxSkeleton 'PlutusV2, JP.AssetClass)
createRaffle paramsMP rPrize rTicketPrice rMinNoOfTickets rCommitDdl rRevealDddl ownAddr = do
  let pRaffleMintingPolicy = compileRafflesStateTokenMintingPolicyPlutus paramsMP
  let pRaffleMintingPolicyHash = JP.mintingPolicyHash pRaffleMintingPolicy
  let gyRaffleMintignPolicy = mintingPolicyFromPlutus @ 'PlutusV2 pRaffleMintingPolicy
  let pRaffleValidator = compileRaffleValidatorPlutus (RaffleValidatorParams pRaffleMintingPolicyHash)
  let pRaffleValidatorHash = JP.validatorHash pRaffleValidator
  let gyRaffleValidator = validatorFromPlutus @ 'PlutusV2 pRaffleValidator
  gyOrganizerPKH <- addressToPubKeyHash' ownAddr
  let pOrganizerPKH = pubKeyHashToPlutus gyOrganizerPKH
  oref <- txOutRefToPlutus <$> someUTxO
  let stateTokenAssetClass = JP.AssetClass (JP.scriptCurrencySymbol pRaffleMintingPolicy, tokenNameFromTxOutRef oref)
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
  return
    ( mconcat
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
    , stateTokenAssetClass
    )

------------------------

-- *  Cancel Raffle Transaction

------------------------

cancelRaffle ::
  (HasCallStack, GYTxMonad m, GYTxQueryMonad m) =>
  --  | Reference Script.
  GYTxOutRef ->
  -- | Raffle minting policy params
  RaffleParams ->
  JP.AssetClass ->
  m (GYTxSkeleton 'PlutusV2)
cancelRaffle refScript paramsMP raffleID = do
  let pRaffleMintingPolicy = compileRafflesStateTokenMintingPolicyPlutus paramsMP
  let pRaffleMintingPolicyHash = JP.mintingPolicyHash pRaffleMintingPolicy
  let gyRaffleMintignPolicy = mintingPolicyFromPlutus @ 'PlutusV2 pRaffleMintingPolicy
  let pRaffleValidator = compileRaffleValidatorPlutus (RaffleValidatorParams pRaffleMintingPolicyHash)
  let pRaffleValidatorHash = JP.validatorHash pRaffleValidator
  let gyRaffleValidator = validatorFromPlutus @ 'PlutusV2 pRaffleValidator
  nId <- networkId

  rvAddress <- scriptAddress gyRaffleValidator
  raffleUtxo <- head . utxosToList . filterUTxOs (utxoHasRaffleStateToken raffleID) <$> utxosAtAddress rvAddress
  let gyraffleDatum = getRaffleInlineDatum raffleUtxo
  let raffleDatum = unsafeFromBuiltinData $ datumToPlutus' gyraffleDatum
  let raffleUtxoValue = valueToPlutus $ utxoValue raffleUtxo
  donationValue <- valueFromPlutus' $ raffleUtxoValue #- getRaffleMandatoryTotalValue raffleDatum
  gydonationPKH <- pubKeyHashFromPlutus' (donationPKH . raffleParams $ raffleDatum)
  let donationAddress = addressFromPubKeyHash nId gydonationPKH
  gyPrizeVal <- valueFromPlutus' (rafflePrizeValue raffleDatum)
  gyOrganizerPKH <- pubKeyHashFromPlutus' (raffleOrganizer raffleDatum)
  let gyOrganizerAddress = addressFromPubKeyHash nId gyOrganizerPKH
  now <- currentSlot
  gyTokenName <- tokenNameFromPlutus' (snd . JP.unAssetClass $ raffleID)
  after36h <- advanceSlot' now 129600
  commitDdllSlot <- gySlotFromPOSIXTime (raffleCommitDeadline raffleDatum)
  return $
    mconcat $
      ( if isEmptyValue donationValue
          then mempty
          else
            mustHaveOutput
              GYTxOut
                { gyTxOutAddress = donationAddress
                , gyTxOutDatum = Nothing
                , gyTxOutValue = donationValue
                , gyTxOutRefS = Nothing
                }
      ) :
      [ isInvalidBefore now
      , isInvalidAfter (minimum [after36h, commitDdllSlot])
      , mustMint gyRaffleMintignPolicy (redeemerFromPlutusData $ RaffleStateThreadNFT.Burning raffleDatum) gyTokenName (negate 1)
      , mustHaveOutput
          GYTxOut
            { gyTxOutAddress = gyOrganizerAddress
            , gyTxOutDatum = Nothing
            , gyTxOutValue = gyPrizeVal
            , gyTxOutRefS = Nothing
            }
      , mustHaveInput
          GYTxIn
            { gyTxInTxOutRef = utxoRef raffleUtxo
            , gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyRaffleValidator) gyraffleDatum (redeemerFromPlutusData Cancel)
            }
      , mustBeSignedBy gyOrganizerPKH
      ]

getRaffleInlineDatum :: GYUTxO -> GYDatum
getRaffleInlineDatum utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> datum
  _ -> error "Invalid datum"

utxoHasRaffleStateToken :: JP.AssetClass -> GYUTxO -> Bool
utxoHasRaffleStateToken (JP.AssetClass (cs, tn)) GYUTxO {..} = isInValue (cs, tn, 1) $ valueToPlutus utxoValue

