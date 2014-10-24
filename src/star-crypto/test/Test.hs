import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import StarVote.Crypto.Math


asIntegerModSelectsLeastNonnegativeRepresentative (Positive m) a =
  0 <= a' && a' < m
  where (Just a') = asIntegerMod m a

unsafeExpModAgreesWithNaive (Positive m) b (NonNegative e) =
  unsafeExpMod m b e == (b ^ e) `mod` m

extendedGcdAgreesWithPrelude a b =
    g == gcd a b where (g, x, y) = extendedGcd a b

modInverseReturnsInverses (Positive m) a =
    0 /= a `mod` m
      &&
    1 == gcd m a
  ==>
    1 `mod` m == a * a' `mod` m
  where a' = unsafeModInverse m a


mathTests =
  testGroup "StarVote.Crypto.Math" [
        testProperty
          "expMod agrees with naive modular exponentiation"
          unsafeExpModAgreesWithNaive
      , testProperty
          "extendedGcd agrees with Prelude.gcd"
          extendedGcdAgreesWithPrelude
      , testProperty
        "asIntegerMod m selects the least nonnegative coset representative in Z/mZ"
          asIntegerModSelectsLeastNonnegativeRepresentative
      , testProperty
          "modInverse returns multiplicative inverses in Z/mZ"
          modInverseReturnsInverses
      ]


main :: IO ()
main =
  defaultMain [
      mathTests
  ]
