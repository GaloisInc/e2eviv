import Test.Framework (defaultMain)

import Test.ElGamal
import Test.Math
import Test.ThresholdElGamal


main :: IO ()
main =
  defaultMain [
      elGamalTests
    , mathTests
    -- , thresholdElGamalTests
    ]
