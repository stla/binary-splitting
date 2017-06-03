module BinarySplitting
  where
import Data.Ratio ((%))


split1 :: [Rational] -> [Rational]
split1 beta = map (\i -> (beta !! (2*i)) * (beta !! (2*i+1))) [0 .. m]
  where n = length beta
        m = div n 2 - 1

split2 :: ([Rational], [Rational], [Rational]) -> [Rational]
split2 adb = map
              (\i -> (alpha !! (2*i)) * (beta !! (2*i+1)) +
                     (delta !! (2*i)) * (alpha !! (2*i+1)))
              [0 .. m]
  where (alpha, delta, beta) = adb
        n = length alpha
        m = div n 2 - 1

split3 :: ([Rational], [Rational], [Rational]) -> ([Rational], [Rational], [Rational])
split3 adb = split adb (length alpha)
  where (alpha, _, _) = adb
        split :: ([Rational], [Rational], [Rational]) -> Int -> ([Rational], [Rational], [Rational])
        split uvw n =
          if n == 1
            then uvw
            else split (split2 uvw, split1 v, split1 w) (div n 2)
          where (u, v, w) = uvw

splitting :: [Rational] -> [Rational] -> Rational
splitting u v = num / den + 1
  where ([num], _, [den]) = split3 (alpha, delta, beta)
        alpha = u
        delta = u
        beta = v

piapprox :: Int -> Double
piapprox m = 2 * fromRational (splitting u v)
  where u = [i | i <- [1 .. 2^m]]
        v = [2*i+1 | i <- [1 .. 2^m]]
