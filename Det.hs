module Det where
det :: [[Float]] -> Float
det [[a,b],[c,d]] = (a * d) - (c * b) --случай для матриц 2×2
det (xs:xss) = detComp 0 0 xs         --общий случай n×n
  where
      detComp _ sumacc []  = sumacc 
      detComp i sumacc (x:xs') = detComp (i+1) ((+ sumacc) $! ((* ((-1)^i * x)) $! (det (dropped i xss)))) xs' 
      dropped i yss = [[ys !! j | j <- [0 .. length ys - 1], j /= i] | ys <- yss]  
det _ = error "Non-square matrix"     --не квадратные матрицы