module TestRaffle where

import Control.Monad
import Jambhala.Plutus
import Plutus.Model

-- | alocate 3 users with 1000 lovelaces each
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ adaValue 1000

simpleSpend :: Run Bool
simpleSpend = do
  users <- setupUsers -- create 3 users and assign each 1000 lovelaces
  let [u1, u2, u3] = users -- give names for users
  sendValue u1 (adaValue 100) u2 -- send 100 lovelaces from user 1 to user 2
  sendValue u2 (adaValue 100) u3 -- send 100 lovelaces from user 2 to user 3
  isOk <- noErrors -- check that all TXs were accepted without errors
  vals <- mapM valueAt users -- read user values
  pure $
    isOk && (vals == fmap adaValue [900, 1000, 1100])

mymock :: Mock
mymock = initMock defaultAlonzo (adaValue 100000)
a :: (Bool, Mock)
a = runMock simpleSpend mymock

-- >>> fst a
-- True
-- True
