module StarVote.Crypto.Types where

import Data.Array (Array)

data TEGParams
  = TEGParams {
        tegGroup     :: DiffieHellmanGroup
      , tegTrustees  :: Integer
      , tegThreshold :: Integer
      }

-- TODO: Write smart constructor that checks
-- that 0 < tegThreshold <= tegTrustees
makeTEGParams = undefined

data TEGPublicKey = TEGPublicKey TEGParams Integer

data TEGPrivateKey = TEGPrivateKey TEGParams Integer

data TEGCipherText = TEGCipherText Integer Integer

data Shares = Shares (Array Integer Integer)

data DiffieHellmanGroup = DiffieHellmanGroup {
    dhgSize      :: Integer
  , dhgOrder     :: Integer
  , dhgGenerator :: Integer
  }

-- TODO: Write smart constructor that checks that:
-- -- `size` is positive
-- -- `order` is probable prime
-- -- `generator` is an integer in [2, order)
makeDiffieHellmanGroup :: Integer
                       -> Integer
                       -> Integer
                       -> DiffieHellmanGroup
makeDiffieHellmanGroup = undefined
