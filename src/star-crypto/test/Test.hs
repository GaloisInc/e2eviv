import Test.Framework (defaultMain)

import Test.Math
import Test.ThresholdElGamal


main :: IO ()
main =
  defaultMain [
      mathTests
    , thresholdElGamalTests
    ]
