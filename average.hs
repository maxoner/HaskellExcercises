module Average where

avg :: Int -> Int -> Int -> Double
avg x y z = (fromInteger (sum $ map toInteger [x,y,z]) :: Double) / 3
{-
Здесь мы явно привели к Double используя fromInteger , у которого мы
нужным образом типизировали выходное значение от применения к Double.
Важно также привести к Integer не сумму Int'ов, а каждый по отдельности
вдруг, например, Int переполнится после суммирования.
Для так как Int принадлежит классу типов Integral, можно переписать
проще 
-}
avg' :: Int -> Int -> Int -> Double
avg' x y z = (sum $ map fromIntegral [x,y,z]) / 3 
{-
Функция fromIntegral полиморфна и отображает любой Integral в любой Num.
Без явной типизации даст полиморфный результат относительно класса Num
    GHCI>:t fromIntegral 3
    Num a => a 
Однако, типизировав изначально функцию avg', мы типизировали и fromIntegral :: Int -> Double
-}