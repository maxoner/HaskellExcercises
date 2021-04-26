{-# LANGUAGE FlexibleContexts #-}
module Odds where

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = [] 
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs

printVals :: (Integral a, Show a) => a -> IO [()]
printVals n = mapM picker $! reverse [1..n]
  where
    picker x 
      |x `mod` 5 == 0 = putStrLn $ show x ++ "\nBeep"
      |otherwise =putStrLn $ show x