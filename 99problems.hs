myLast (x:[]) = x
myLast (x:xs) = myLast xs

myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt (l:ls) 1 = l
elementAt (l:ls) n = elementAt ls (n-1)

myLength x = hack x 0
  where
    hack [] n = n
    hack (x:xs) n = hack xs (n+1)

myLength2 = foldl (\x y -> x+1) 0

myReverse = foldl (flip (:)) []

myReverse2 x = hack x []
  where
    hack [] r = r
    hack (x:xs) r = hack xs (x:r)

fac = scanl (*) 1 [1..]
