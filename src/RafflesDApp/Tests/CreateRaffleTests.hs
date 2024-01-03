module RafflesDApp.Tests.CreateRaffleTests where

import GeniusYield.Api.TestTokens (mintTestTokens)

import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Jambhala.Plutus (tokenName)
import Jambhala.Plutus qualified

import Plutus.Model (logInfo)
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
  GYTxMonadRun GYTxId
createRaffleRun paramsMP rPrize rTicketPrice rMinNoOfTickets rCommitDdl rRevealDddl =
  do
    addr <- ownAddress
    skeleton <- createRaffle paramsMP rPrize rTicketPrice rMinNoOfTickets rCommitDdl rRevealDddl addr
    sendSkeleton skeleton

mintTestsTokenRun :: GYTokenName -> Integer -> GYTxMonadRun GYValue
mintTestsTokenRun tn i = do
  (ac, skeleton) <- mintTestTokens tn (fromInteger i)
  void $ sendSkeleton skeleton
  return $ valueSingleton ac i

createRaffleTrace :: Wallets -> Run ()
createRaffleTrace Wallets {..} = do
  --First step: Get the required parameter

  mintedTestTokens <- runWallet w1 $ do
    testTokens <- tokenNameFromPlutus' (tokenName "AlaBalaPortocala")
    mintTestsTokenRun testTokens 100
  finalBalance <- runWallet w1 $ do
    cddl <- pPOSIXTimeFromSlotInteger 13
    rddl <- pPOSIXTimeFromSlotInteger 15
    void $ createRaffleRun sampleRafflePrams (valueToPlutus (fromJust mintedTestTokens)) 10_000_000 5 cddl rddl
    balance w1

  logInfo (show finalBalance)
  return ()

runTest :: IO ()
runTest = defaultMain $ testGroup "CreateRaffles" [createRaffleTests]
