module Main where

import RafflesDApp.Tests.CreateRaffleTests ( runTest )
main :: IO ()
main = runTest

-- import Contracts (contracts, samples)
-- import Jambhala.CLI (runJamb)

-- main :: IO ()
-- main = runJamb allContracts -- << replace `allContracts` with `contracts` to hide sample contracts
--   where
--     allContracts = contracts <> samples
