module StarVote.Crypto.ElGamal where

import Crypto.Classes
import Crypto.Random
import Control.Monad.CryptoRandom

-- import Data.Array as A
-- import qualified Data.ByteString as B

import StarVote.Crypto.Math
import StarVote.Crypto.Types

-- Reference:
--   [HAC] Menezes, van Oorschot, Vanstone.
--         "Handbook of Applied Cryptography".
--          CRC Press, 1996
--          www.cacr.math.uwaterloo.ca/hac

-- Key generation for ElGamal public-key encryption.
-- [HAC 294] Algorithm 8.17
buildKeyPair :: (CryptoRandomGen rng)
             => rng
             -> TEGParams
             -> Either GenError ((TEGPublicKey, TEGPrivateKey), rng)
buildKeyPair rng params = do
  let (k, p, g) = dhParams params
      bl = div k 8
      lb = 1
      ub = p - 2
  (privateExponent, rng') <- crandomR (lb, ub) rng
  let publicKey  = TEGPublicKey  params (unsafeExpMod p g privateExponent)
      privateKey = TEGPrivateKey params privateExponent
  return ((publicKey, privateKey), rng')

-- ElGamal public-key encryption (Encryption).
-- [HAC 295] Algorithm 8.18.1
encryptAsym :: (CryptoRandomGen rng)
            => rng
            -> TEGPublicKey
            -> Integer
            -> Either GenError (TEGCipherText, rng)
encryptAsym rng0 (TEGPublicKey params halfSecret) msg = do
  let (_, p, g) = dhParams params
      lb = 1
      ub = p - 2
  (privateExponent, rng1) <- crandomR (lb, ub) rng0
  let gamma = unsafeExpMod p g privateExponent
      delta = (msg * (unsafeExpMod p halfSecret privateExponent)) `mod` p
  return (TEGCipherText gamma delta, rng1)

-- ElGamal public-key encryption (Decryption).
-- [HAC 295] Algorithm 8.18.2
decryptAsym :: TEGPrivateKey
            -> TEGCipherText
            -> Integer
decryptAsym privateKey ctxt = (gamma' * delta) `mod` p
  where (TEGPrivateKey params privateExponent) = privateKey
        (TEGCipherText gamma delta) = ctxt
        (_, p, g) = dhParams params
        gamma' = unsafeModInverse p (unsafeExpMod p gamma privateExponent)
