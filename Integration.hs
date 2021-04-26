module Integration where
{-# LANGUAGE UnicodeSyntax #-}
import Data.Function

integrateMap f a b = h * ((f a + f b) / 2 + sum ( map f xs))
  where
    h = (b - a) / n
    n = 10 ^ 6
    xs = [a + h * x| x <- [1..n-1]]

integrateCount f a b =summa 0 a 0
  where
   summa acc a' count
          |count == grid = acc
          |otherwise = summa (acc + trapz a') (a' + step) (count + 1)
    trapz a' = (f a' + f (a' + step)) * step / 2
    step = (b - a) / grid
    grid = 10 ^6
