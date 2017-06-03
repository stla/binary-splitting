module Exponential
  where
import BinarySplitting
import Data.Ratio ((%))

exponential :: Int -> Rational -> (Rational, Double)
exponential m x = (result, fromRational result)
  where u = replicate (fromInteger n) x
        v = [i % one | i <- [1 .. n]]
        n = 2^m
        one = 1::Integer
        result = bsplitting u v

exp1 :: Int -> (Rational, Double)
exp1 m = exponential m 1
