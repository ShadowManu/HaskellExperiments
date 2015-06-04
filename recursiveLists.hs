hamilton = 1:merge (merge (map (2*) hamilton) (map (3*) hamilton)) (map (5*) hamilton)
  where
    merge (a:x) (b:y) | a<b       = a : merge x (b:y)
                      | a>b       = b : merge (a:x) y
                      | otherwise = a : merge x y

ha = 1:merge (merge (map (2*) [1..]) (map (3*) [1..])) (map (5*) [1..])
  where
    merge (a:x) (b:y) | a<b       = a : merge x (b:y)
                      | a>b       = b : merge (a:x) y
                      | otherwise = a : merge x y

hamilton2 = 1:merge (map (2*) hamilton2) (merge (map (3*) hamilton2) (merge (map (5*) hamilton2) (map (7*) hamilton2)))
  where
    merge (a:x) (b:y) | a<b       = a : merge x (b:y)
                      | a>b       = b : merge (a:x) y
                      | otherwise = a : merge x y

hamilton3 = 1:merge (map (2*) hamilton3) (map (3*) hamilton3)
  where
    merge (a:x) (b:y) | a<b       = a : merge x (b:y)
                      | a>b       = b : merge (a:x) y
                      | otherwise = a : merge x y

-- Merge de multiplicaciones de hamilton
mergeGlob (a:x) (b:y)
  | a<b       = a : mergeGlob x (b:y)
  | a>b       = b : mergeGlob (a:x) y
  | otherwise = a : mergeGlob x y

-- Aplica una funcion de una lista de par en par
apply :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
apply f [x,y] = f x y
apply f (x:xs) = f x (apply (f) xs)

-- Dada una lista de enteros y hamilton, devuelva la multiplicaciones de hamilton
mapHamil :: [Int] -> [Int] -> [[Int]]
mapHamil x y = zipWith (f) l1 l2
  where
    f f1 lis = map (f1) lis
    l1 = map (\num -> (*num)) x
    l2 = repeat y

manuelHamil :: [Int] -> [Int]
manuelHamil x = apply (f) (mulHamil (1:manuelHamil x)) 
  where
    f = mergeGlob
    mulHamil = mapHamil x