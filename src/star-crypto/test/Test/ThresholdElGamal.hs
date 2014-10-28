module Test.ThresholdElGamal (thresholdElGamalTests) where


import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Crypto.Random

import StarVote.Crypto.Groups
import StarVote.Crypto.Types
import StarVote.Crypto.ThresholdElGamal
import StarVote.Crypto.ElGamal


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

decryptAfterEncryptIsIdentity :: NonNegative Integer -> Property
decryptAfterEncryptIsIdentity (NonNegative msg) = monadicIO $ do
  rng0   <- run (newGenIO :: IO SystemRandom)
  params <- pick (arbitrary :: Gen TEGParams)
  let roundTrip = do
        ((pk, sk), rng1) <- buildKeyPair rng0 params
        (ctxt, rng2)     <- encryptAsym rng1 pk msg
        let (TEGPrivateKey _ sk') = sk
        shares           <- Right $ buildModifiedShares rng2 params sk'
        return $ decryptWithModifiedShares params shares ctxt
  assert $ case roundTrip of
    Left  err  -> False
    Right ptxt -> False -- ptxt == msg


thresholdElGamalTests =
  testGroup "StarVote.Crypto.ThresholdElGamal" [
    testProperty
      "Decrypt with shares after encrypt is identity"
      decryptAfterEncryptIsIdentity
  ]
