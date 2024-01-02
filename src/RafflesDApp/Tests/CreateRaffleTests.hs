module RafflesDApp.Tests.CreateRaffleTests where

import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Imports
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Jambhala.Plutus (tokenName)
import Jambhala.Plutus qualified
import Plutus.Model
import RafflesDApp.OffChain.Operations
import RafflesDApp.OnChain.RaffleStateThreadNFTMintingPolicy
import RafflesDApp.OnChain.RaffleValidator
import Test.Tasty (TestTree, defaultMain, testGroup)

-- | Our unit tests for creating a raffle
createRaffleTests :: TestTree
createRaffleTests =
  testGroup
    "Create raffle"
    [ testRun "Balance checks creating first raffle" createRaffleTrace
    ]

createRaffleRun ::
  RaffleParams ->
  RaffleValidatorParams ->
  RaffleDatum ->
  Jambhala.Plutus.ValidatorHash ->
  GYTxMonadRun GYTxId
createRaffleRun mpParams valParams raffle valHash =
  do
    addr <- ownAddress
    skeleton <- createRaffle addr mpParams valParams raffle valHash
    sendSkeleton skeleton

mintStateTokenRun :: GYTokenName -> GYTxMonadRun GYAssetClass
mintStateTokenRun tn = do
  (ac, skeleton) <- mintTestTokens tn 1
  sendSkeleton skeleton
  return ac

createRaffleTrace :: Wallets -> Run ()
createRaffleTrace ws@Wallets {..} = do
  --   --First step: Get the required parameter
  --   let tn1 = fromJust $ tokenNameFromPlutus (tokenName "alabala")
  --   void $ runWallet w1 $ mintStateTokenRun tn1
  void $
    -- following operations are ran by first wallet, `w1`
    runWallet w1 $
      do
        createRaffleRun sampleRafflePrams sampleRaffleValidatorParams (sampleRaffleNew {rafflePrizeValue = lovelaceValueOf 7_100_000}) sampleRaffleValidatorHash

runTest = defaultMain $ testGroup "CreateRaffles" [createRaffleTests]

-- -- >>> run
