module Sum3 where

{-Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом закончившиеся списки не должны давать вклада в суммы.-}

sum3 :: (Num a, Eq a) => [a] -> [a] -> [a] ->[a]
sum3 [] [] [] = []
sum3 a b c = 
    let
        summator sums [(a:[]), (b:[]), (c:[])] = (a + b + c):sums
        summator sums [(a:as), (b:bs), (c:cs)] = summator ((a + b + c):sums) $ isempty [as, bs,cs]
        isempty = map (\x -> if x == [] then [0] else x)
    in
        reverse $ summator [] (isempty [a, b, c]) 
