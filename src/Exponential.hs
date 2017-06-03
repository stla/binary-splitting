module Exponential
  where
import BinarySplitting
import Data.Ratio ((%))

exponential :: Int -> Rational -> (Rational, Double)
exponential m x = (result, fromRational result)
  where u = replicate (2^m) x
        v = [i | i <- [1 .. 2^m]]
        result = bsplitting u v

exp1 :: Int -> (Rational, Double)
exp1 m = exponential m 1
