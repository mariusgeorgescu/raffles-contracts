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

mintStateTokenRun :: GYTokenName -> GYTxMonadRun GYAssetClass
mintStateTokenRun tn = do
  (ac, skeleton) <- mintTestTokens tn 10
  void $ sendSkeleton skeleton
  return ac

createRaffleTrace :: Wallets -> Run ()
createRaffleTrace Wallets {..} = do
  --First step: Get the required parameter
  let tn1 = fromJust $ tokenNameFromPlutus (tokenName "alabala")
  ac <- runWallet w1 $ mintStateTokenRun tn1
  finalBalance <- runWallet w1 $ do
    void $ createRaffleRun sampleRafflePrams (valueToPlutus (valueSingleton (fromJust ac) 10)) (raffleTicketPrice sampleRaffleNew) (raffleMinNoOfTickets sampleRaffleNew) (raffleCommitDeadline sampleRaffleNew) (raffleRevealDeadline sampleRaffleNew)
    balance w1
  
  logInfo (show finalBalance)
  return ()

runTest :: IO ()
runTest = defaultMain $ testGroup "CreateRaffles" [createRaffleTests]
