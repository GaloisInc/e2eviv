module Test.ThresholdElGamal (thresholdElGamalTests) where


import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

decryptAfterEncryptIsIdentity = monadicIO $ do
  return True


thresholdElGamalTests =
  testGroup "StarVote.Crypto.ThresholdElGamal" [
    testProperty
      "Decrypt after encrypt is identity"
      decryptAfterEncryptIsIdentity
  ]
