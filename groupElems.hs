module GroupElemns where
-- Напишите функцию groupElems, которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.

--groupElems :: Eq a => [a] -> [[a]]
--groupElems (a:as) = reverse $ foldl helpr [[a]] as
 -- where
   -- helpr x y  = if (head . head) x == y then y:(head x) else [y] ++ x
   -- 
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = reverse $ gr [[x]] xs
  where
    gr xs [] = xs
    gr xs y 
      |(head . head) xs == head y = gr ([head y:head xs] ++ tail xs) (tail y)
      |otherwise = gr ([[head y]] ++ xs) (tail y)
