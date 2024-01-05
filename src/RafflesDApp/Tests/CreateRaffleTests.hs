module RafflesDApp.Tests.CreateRaffleTests where

import GeniusYield.Api.TestTokens (mintTestTokens)

import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Jambhala.Plutus (tokenName)
import Jambhala.Plutus qualified

import Data.List.Extra (replicate)
import Jambhala.Plutus qualified as JP
import Plutus.Model (logInfo, waitNSlots)
import RafflesDApp.OffChain.Operations
import RafflesDApp.OnChain.RaffleStateThreadNFTMintingPolicy
import RafflesDApp.OnChain.RaffleValidator
import Test.Tasty (TestTree, defaultMain, testGroup)

-- | Our unit tests for creating a raffle
createRaffleTests :: TestTree
createRaffleTests =
  testGroup
    "Raffle Scenarios"
    [ testRun "Create a raffle and cancel it " $ createRaffleAndCancelTrace sampleRafflePrams
    ]

createRaffleRun ::
  RaffleParams ->
  Jambhala.Plutus.Value ->
  Integer ->
  Integer ->
  Jambhala.Plutus.POSIXTime ->
  Jambhala.Plutus.POSIXTime ->
  GYTxMonadRun JP.AssetClass
createRaffleRun paramsMP rPrize rTicketPrice rMinNoOfTickets rCommitDdl rRevealDddl =
  do
    addr <- ownAddress
    (skeleton, raflleId) <- createRaffle paramsMP rPrize rTicketPrice rMinNoOfTickets rCommitDdl rRevealDddl addr
    void $ sendSkeleton skeleton
    return raflleId

cancelRaffleRun ::
  GYTxOutRef ->
  -- | Raffle minting policy params
  RaffleParams ->
  JP.AssetClass ->
  GYTxMonadRun GYTxId
cancelRaffleRun refScript paramsMP raffleID = do
  skeleton <- cancelRaffle refScript paramsMP raffleID
  sendSkeleton skeleton

mintTestsTokenRun :: GYTokenName -> Integer -> GYTxMonadRun GYValue
mintTestsTokenRun tn i = do
  (ac, skeleton) <- mintTestTokens tn (fromInteger i)
  void $ sendSkeleton skeleton
  return $ valueSingleton ac i

----------------------
-- ACTIONS
-----------------------

deployRaffleValidatorTrace :: RaffleParams -> Wallet -> GYAddress -> Run GYTxOutRef
deployRaffleValidatorTrace params from to = do
  let gyRaffleValidator = getGYRaffleValidator params
  valRef <- runWallet' from $ addRefScript to gyRaffleValidator
  logInfo' "VALIDATOR DEPLOYED"
  case valRef of
    Nothing -> error "failed to add the reference script"
    Just gtor -> return gtor

createRaffleTrace :: RaffleParams -> Wallet -> GYValue -> Integer -> Integer -> Integer -> Integer -> Run JP.AssetClass
createRaffleTrace params from prize ticketPrice minTickets commitSlot revealSlot = do
  raffleId <- runWallet' from $ do
    cddl <- pPOSIXTimeFromSlotInteger commitSlot
    rddl <- pPOSIXTimeFromSlotInteger revealSlot
    createRaffleRun params (valueToPlutus prize) ticketPrice minTickets cddl rddl

  logInfo' ("CREATED :" ++ show raffleId)
  return raffleId

----------------------
-- SCENARIOS
-----------------------

createRaffleAndCancelTrace :: RaffleParams -> Wallets -> Run ()
createRaffleAndCancelTrace params Wallets {..} = do
  --Deploy the validator to be used as reference script
  refRaffleValidator <- deployRaffleValidatorTrace params w1 (walletAddress w9)

  --Mint some test tokens to be used as Raffle Prize
  mintedTestTokens <- runWallet' w1 $ do
    testTokens <- tokenNameFromPlutus' (tokenName "AlaBalaPortocala")
    mintTestsTokenRun testTokens 100
  logInfo' "TEST TOKENS MINTED"

  --Create the raffle
  raffleId <- createRaffleTrace params w1 mintedTestTokens 1 5 16 30
  waitNSlots 3
  -- Slot 16 - Right before commit deadline ;)

  --Cancel the raffle
  _ <- runWallet' w1 $ do
    cancelRaffleRun refRaffleValidator params raffleId
  return ()

logInfo' :: String -> Run ()
logInfo' s =
  logInfo
    ( "\n"
        ++ replicate 100 '='
        ++ "\n"
        ++ s
        ++ "\n"
        ++ replicate 100 '='
        ++ "\n"
    )

runTest :: IO ()
runTest = defaultMain $ testGroup "CreateRaffles" [createRaffleTests]
