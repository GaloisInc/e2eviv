module Test.ThresholdElGamal (thresholdElGamalTests) where

import qualified Data.ByteString.Char8 as B

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Crypto.Random

import StarVote.Crypto.Groups
import StarVote.Crypto.Types
import StarVote.Crypto.ThresholdElGamal


positiveBoundedBy :: Integer -> Positive Integer -> Bool
positiveBoundedBy b (Positive x) = x <= b

instance Arbitrary TEGParams where
  arbitrary = do
    (Positive trustees)  <- arbitrary
    (Positive threshold) <- arbitrary `suchThat` positiveBoundedBy trustees
    return TEGParams {
        tegGroup     = modp2048
      , tegTrustees  = trustees
      , tegThreshold = threshold
      }

decryptAfterEncryptIsIdentity :: Positive Integer -> Property
decryptAfterEncryptIsIdentity (Positive msg) = monadicIO $ do
  rng    <- run (newGenIO :: IO SystemRandom)
  params <- pick (arbitrary :: Gen TEGParams)
  let roundTrip = do
        ((pk, sk), rng') <- buildKeyPair rng params
        (ctxt, _)        <- encryptAsym rng' pk msg
        return $ decryptAsym sk ctxt
  return $ case roundTrip of
    Left  err  -> False
    Right ptxt -> ptxt == msg


thresholdElGamalTests =
  testGroup "StarVote.Crypto.ThresholdElGamal" [
    testProperty
      "Decrypt after encrypt is identity"
      decryptAfterEncryptIsIdentity
  ]
