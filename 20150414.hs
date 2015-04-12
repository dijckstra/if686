compose :: (t -> t) -> [(t -> t)] -> [(t -> t)]
compose f [] = []
compose f (g:gs) = [fog] ++ (compose f gs)
 where fog x = f (g x)