module StarVote.Crypto.Math where

import Data.Array as A
import Data.Maybe (fromJust)


-- Computes modular exponentation via right-to-left binary algorithm.
-- We are computing in Z/mZ, so we require that m be positive.
-- Since b^(-1) exists just when gcd(b, m) == 1, and this is only guaranteed
-- when m is prime, we leave the result undefined for negative exponents.
expMod
  :: Integer
  -> Integer
  -> Integer
  -> Maybe Integer
expMod m b e
  | m <= 0    = Nothing
  | e < 0     = Nothing
  | m == 1    = Just 0
  | e == 0    = Just 1
  | otherwise = Just $ next b e 1
  where
    next _ 0 acc = mod acc m
    next b e acc = next b' e' acc'
      where
        b' = mod (b * b) m
        e' = div e 2
        acc' = if even e
               then acc
               else mod (acc * b) m

unsafeExpMod m b e = fromJust (expMod m b e)

-- Computes GCD using extended euclidean algorithm
-- as described in Stein's "ENT", p. 33, Algorithm 2.3.7.
extendedGcd
  :: Integer
     -- a
  -> Integer
     -- b
  -> (Integer, Integer, Integer)
     -- The triple (g, x, y) s.t. g = ax + by
extendedGcd a b
  | a > b     = next a b r s x y
  | otherwise = next b a r s x y
  where (r, s, x, y) = (0, 1, 1, 0)
        next g 0 r s x y = (g, x, y)
        next a b r s x y = next b
                                (rem a b)
                                (x - (div a b) * r)
                                (y - (div a b) * s)
                                r
                                s

-- Recall that the multiplicative group of units (Z/mZ)*
-- is given by those `a` coprime to m, i.e. gcd(a, m) = 1.
-- Thus, the modular inverse is in general partial on Integer.
modInverse :: Integer -> Integer -> Maybe Integer
modInverse m a
  | g == 1    = Just y  -- Since mx + ay = 1
                        --   ==> ay = 1 - mx
                        --   ==> ay = 1 (mod m)
                        --   ==> y = a^(-1) (mod m)
  | otherwise = Nothing
  where (g, x, y) = extendedGcd m a

unsafeModInverse m = fromJust . (modInverse m)

-- Represents a polynomial in Z or Z/mZ.
-- (!) Interpretation of the specific ring is determined by the caller.
data Polynomial = Polynomial (Array Integer Integer)

-- Evaluate a polynomial, applying `f` to each evaluated term before summing.
evalPolyWith f (Polynomial poly) x = sum [ f t
                                         | let (lb, ub) = bounds poly,
                                           i <- [lb..ub],
                                           let t = (poly ! i) * (x ^ i)
                                         ]

-- Usual univariate polynomial evaluation.
evalPoly = evalPolyWith id

-- Evaluation of polynomial in the ring Z/mZ.
evalPolyMod m = evalPolyWith (`mod` m)
