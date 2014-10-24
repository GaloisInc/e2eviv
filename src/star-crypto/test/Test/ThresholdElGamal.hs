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

-- TODO: make PropertyM, IO, and Either cohere so no indent drift
decryptAfterEncryptIsIdentity :: Positive Integer -> Property
decryptAfterEncryptIsIdentity (Positive msg) = monadicIO $ do
  rng     <- run (newGenIO :: IO SystemRandom)
  params  <- pick (arbitrary :: Gen TEGParams)
  keyPair <- run $ return $ buildKeyPair rng params
  case keyPair of
    Left err               -> return False
    Right ((pk, sk), rng') -> do
      cipherText <- run $ return $ encryptAsym rng' pk msg
      case cipherText of
        Left err -> return False
        Right (c, _)  -> do
          p <- return $ decryptAsym sk c
          return $ p == msg


thresholdElGamalTests =
  testGroup "StarVote.Crypto.ThresholdElGamal" [
    testProperty
      "Decrypt after encrypt is identity"
      decryptAfterEncryptIsIdentity
  ]
