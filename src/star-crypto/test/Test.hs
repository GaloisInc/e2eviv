import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import StarVote.Crypto.Math


unsafeExpModAgreesWithNaive (Positive m) b (NonNegative e) =
  unsafeExpMod m b e == (b ^ e) `mod` m


mathTests =
  testGroup "StarVote.Crypto.Math" [
        testProperty
          "expMod agrees with naive modular exponentiation"
          unsafeExpModAgreesWithNaive
      ]


main :: IO ()
main =
  defaultMain [
      mathTests
  ]
