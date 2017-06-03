module ApproxPi
  where
import BinarySplitting

approxPi :: Int -> (Rational, Double)
approxPi m = (result, fromRational result)
  where u = [i | i <- [1 .. 2^m]]
        v = [2*i+1 | i <- [1 .. 2^m]]
        result = 2 * bsplitting u v
