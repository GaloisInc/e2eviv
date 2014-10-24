import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import StarVote.Crypto.Math


unsafeExpModAgreesWithNaive (Positive m) b (NonNegative e) =
  unsafeExpMod m b e == (b ^ e) `mod` m

extendedGcdAgreesWithPrelude a b =
    g == gcd a b where (g, x, y) = extendedGcd a b


mathTests =
  testGroup "StarVote.Crypto.Math" [
        testProperty
          "expMod agrees with naive modular exponentiation"
          unsafeExpModAgreesWithNaive
      , testProperty
          "extendedGcd agrees with Prelude.gcd"
          extendedGcdAgreesWithPrelude
      ]


main :: IO ()
main =
  defaultMain [
      mathTests
  ]
