import Test.Framework (defaultMain)

import Test.Math


main :: IO ()
main =
  defaultMain [
      mathTests
  ]
