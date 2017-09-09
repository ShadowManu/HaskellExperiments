import Data.List (foldl1')

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

-- New Definitions

-- Like mergesort's merge, joins 2 lists always putting up the lower first. Removes duplicates.
nmerge :: [Int] -> [Int] -> [Int]
nmerge xs [] = xs
nmerge [] ys = ys
nmerge (a:x) (b:y)
  | a<b       = a : nmerge x (b:y)
  | a>b       = b : nmerge (a:x) y
  | otherwise = a : nmerge x y

-- Apply a binary function over items a list sequentially
napply :: (a -> a -> a) -> [a] -> a
napply = foldr1

-- Returns a list for every number in the first list multiplying the second
nmult :: [Int] -> [Int] -> [[Int]]
nmult mults nums = [ map (*m) nums | m <- mults ]

-- Hamilton list for a list of numbers
nhamil :: [Int] -> [Int]
nhamil mults = napply nmerge (nmult mults [1..]) 

-- Playing with factors

ndivisors :: Int -> [Int]
ndivisors n = [ d | d <- [2..n-1], n `mod` d == 0]

nfactors :: Int -> [Int]
nfactors num = if null divs then [num] else foldl1' nmerge $ map nfactors divs 
  where
    divs = ndivisors num

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = mempty
  Nada `mappend` Nada = Nada
  Nada `mappend` Only b  = Only b
  Only a  `mappend` Nada = Only a
  Only a  `mappend` Only b  = Only $ a `mappend` b