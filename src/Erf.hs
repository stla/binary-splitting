module Erf
  where
import BinarySplitting
import Hypergeo1F1
import Exponential
import Data.Ratio ((%))

erf :: Int -> Rational -> Double
erf m x = (fromRational $
  2 * x * hypergeo1F1 m (1%1) (3%2) (x^2) *
    expo m (-x^2)) / sqrt pi
    
erf1 :: Int -> Rational -> Double
erf1 m x = (fromRational $ 2 * x * hypergeo1F1 m (1%2) (3%2) (-x^2)) / sqrt(pi)

erf11 :: Int -> Rational -> Double
erf11 m x = exp (fromRational (-x^2)) *
  (fromRational $ 2 * x * hypergeo1F1 m (1%1) (3%2) (x^2)) / sqrt(pi)


-- https://en.wikipedia.org/wiki/Error_function
erf2 :: Int -> Rational -> Double
erf2 m x = exp (fromRational (-x^2)) / sqrt pi * fromRational (result/x)
  where u = [toInteger (2*i-1) % one | i <- [1 .. n]]
        v = replicate n (-(2*x^2))
        n = 2^m
        one = 1 :: Integer
        result = bsplitting u v
-- erf2 7 10 fonctionne (c'est erfc)

erf3 :: Int -> Rational -> Double
erf3 m x = 2 / sqrt pi * (fromRational (x*result))
  where u = [(toInteger (-2*i+1))%1 * x^2 | i <- [1 .. n]]
        v = [(toInteger i * toInteger (2*i+1))%1 | i <- [1 .. n]]
        n = 2^m
        result = bsplitting u v
