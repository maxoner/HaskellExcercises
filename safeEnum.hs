class (Eq a, Bounded a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
   |x == maxBound = minBound
   |otherwise = succ x 

  spred :: a -> a
  spred x
    |x == minBound = maxBound
    |otherwise = pred x 

func a = a ^ 2