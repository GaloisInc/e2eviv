{-# LANGUAGE
    GADTs,
    MultiParamTypeClasses
  #-}
module StarVote.Crypto.ThresholdElGamal where

import Crypto.Classes
import Crypto.Random
import Control.Monad.CryptoRandom

import Data.Array as A
import qualified Data.ByteString as B

import StarVote.Crypto.Math
import StarVote.Crypto.Types
import StarVote.Crypto.ElGamal

-- Reference:
--   [DF] Yvo Desmedt, Yair Frankel.
--        "Threshold cryptosystems".
--        http://dl.acm.org/citation.cfm?id=118237

-- (!) Throws away rng due to use of `crandomRs`
buildModifiedShares :: (CryptoRandomGen rng)
                    => rng        -- You will lose this!
                    -> TEGParams
                    -> Integer
                    -> Shares
buildModifiedShares rng params secret = Shares $ fmap modifyShare shares
  where
    (_, p, _) = dhParams params
    n = tegTrustees params
    th = tegThreshold params
    lb = 0
    ub = p - 1
    coeffs = take (fromIntegral (th - 1)) $ crandomRs (lb, ub) rng
    poly = Polynomial $ listArray (0, th-1) (secret:coeffs)
    shares = listArray (1, n) $ map (evalPolyMod p poly) [1..n]
    lagrangeBasisAt i = product [ basisFactor
                                | j <- [lb..ub],
                                  i /= j,
                                  let x_j = shares ! j
                                      x_i = shares ! i
                                      basisFactor = x_j `div` (x_j - x_i)
                                ]
    modifyShare i = (lagrangeBasisAt i) * (shares ! i) `mod` p


buildPartialResult :: TEGParams
                   -> Integer
                   -> TEGCipherText
                   -> Integer
buildPartialResult params share ctext = unsafeExpMod p gamma share
  where
    (_, p, _) = dhParams params
    (TEGCipherText gamma delta) = ctext


decryptWithModifiedShares :: TEGParams
                          -> Shares         -- Modified shares
                          -> TEGCipherText  -- Fully unencrypted ciphertext
                          -> Integer
decryptWithModifiedShares params (Shares shares) ctext = ptext
  where
    (_, p, _) = dhParams params
    (TEGCipherText gamma delta) = ctext
    step share = buildPartialResult params share ctext
    pooledModifiedShares = (product $ elems $ fmap step shares) `mod` p
    ptext = (pooledModifiedShares * delta) `mod` p
