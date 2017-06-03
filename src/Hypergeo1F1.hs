module Hypergeo1F1
  where
import BinarySplitting
import Data.Ratio ((%))

hypergeo1F1 :: Int -> Rational -> Rational -> Rational -> (Rational, Double)
hypergeo1F1 m a b x = (result, fromRational result)
  where u = [(a+i)*x | i <- [0 .. 2^m-1]]
        v = [(b+i)*(i+1) | i <- [0 .. 2^m-1]]
        result = bsplitting u v

test_hypergeo1F1 = hypergeo1F1 8 (1%2) (3%2) (-1)