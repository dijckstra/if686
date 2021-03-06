import Data.List (sort, nub)

{---------------------------------------- TRABALHO 8  ----------------------------------------}
listPartitioner :: (Ord a, Num a) => [a] -> ([a] -> [[a]])
listPartitioner xs = (f . sort) xs
 where
 	f [] bs = []
 	f [a] bs = [(sort . filter (<= a)) bs] ++ [(sort . filter (> a)) bs]
 	f (a:as) bs = [(sort . filter (<= a)) bs] ++ f (as) (filter (> a) bs)

{---------------------------------------- EXERCÍCIOS ----------------------------------------}
flipped :: (t -> u -> v) -> (u -> t -> v)
flipped f = (\x y -> f y x)

first :: [(t, u)] -> [t]
first = \pairs -> [x | (x, y) <- pairs]

greater :: (Num a) => [[a]] -> Int -> [[a]]
greater = \as n -> [a | a <- as, (length a) > n]

removeDuplicates :: (Eq a) => [[a]] -> [a]
removeDuplicates list = (removeDuplicates'.concat) list
 where
 	removeDuplicates' = \(x:xs) -> if xs == [] then [] else x : removeDuplicates' (filter (\y -> not ((==) x y)) xs)

sumConst :: (Num a) => a -> [a] -> [a]
sumConst k = map (+k)