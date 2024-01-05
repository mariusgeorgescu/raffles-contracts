module RafflesDApp.Tests.CreateRaffleTests where

import GeniusYield.Api.TestTokens (mintTestTokens)

import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Jambhala.Plutus (tokenName)
import Jambhala.Plutus qualified

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
    "Create raffle"
    [ testRun "Create a raffle" createRaffleTrace
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

createRaffleTrace :: Wallets -> Run ()
createRaffleTrace Wallets {..} = do
  --First step: Get the required parameter

  refRaffleValidator <- runWallet w1 $ do
    let gyRaffleValidator = getGYRaffleValidator sampleRafflePrams
    ref <- addRefScript (walletAddress w9) gyRaffleValidator
    case ref of
      Nothing -> error "failed to add reff script"
      Just gtor -> return gtor
  mintedTestTokens <- runWallet w1 $ do
    testTokens <- tokenNameFromPlutus' (tokenName "AlaBalaPortocala")
    mintTestsTokenRun testTokens 100
  raffleId <- runWallet w1 $ do
    cddl <- pPOSIXTimeFromSlotInteger 20
    rddl <- pPOSIXTimeFromSlotInteger 30
    createRaffleRun sampleRafflePrams (valueToPlutus (fromJust mintedTestTokens)) 10_000_000 5 cddl rddl
  logInfo ("======CREATED" ++ show raffleId)
  waitNSlots 3
  _txID <- runWallet w1 $ do
    cancelRaffleRun (fromJust refRaffleValidator) sampleRafflePrams (fromJust raffleId)
  logInfo "canceled"
  return ()

runTest :: IO ()
runTest = defaultMain $ testGroup "CreateRaffles" [createRaffleTests]
