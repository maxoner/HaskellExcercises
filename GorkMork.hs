module GorkMork where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

{- 
Задайте реализацию по умолчанию метода stompOrStab, которая:
 
 *  вызывает метод stomp, если переданное ему значение приводит в ярость Морка; 
 *  вызывает stab, если оно приводит в ярость Горка
 *  вызывает сначала stab, а потом stomp, если оно приводит в ярость их обоих.

Если не происходит ничего из вышеперечисленного, метод должен возвращать переданный ему аргумент.
-}

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where --это не список, а наследование от двух классов
    stompOrStab :: a -> a
    stompOrStab a
      |doesEnrageMork a && doesEnrageGork a = stomp . stab $ a -- это должно быть первым, в противном случае если пройдена
      |doesEnrageGork a = stab a                                --проверка одного из следущих выражений, то дальше
      |doesEnrageMork a = stomp a                               --проверятся не будет (ленивость мать её!) 
      |otherwise = a

