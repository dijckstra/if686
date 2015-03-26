{- Defina uma tabela hash em termos de: 
um tipo de dados que representa a tabela propriamente dita
e fuņções get, put, remove e hasKey -}
type HashTable = [(Int, Int)]

{- devolve um elemento associado a uma chave -}
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk 
 | hasKey ht kk == False = -1
 | otherwise = snd (ht!!(linearProbe ht kk 0))

full :: HashTable -> Bool
full ht = length ht == sum [1 | (k, v) <- ht, k /= 0]

{- incluem na tabela um elemento associado a uma chave -}
put :: HashTable -> Int -> Int -> HashTable
put ht kk vv
 | full ht = error "tabela is full"
 | hasKey ht kk == False = (take emptyPos ht) ++ [(kk, vv)] ++ (drop (emptyPos + 1) ht)
 | otherwise = (take pos ht) ++ [(kk, vv)] ++ (drop (pos + 1) ht)
 where 
 	emptyPos = linearProbe (ht) (0) (0)
 	pos = linearProbe ht kk 0

{- removem um elemento associado a uma chave e a própria chave -}
remove :: HashTable -> Int -> HashTable
remove ht kk
 | not (hasKey ht kk) = error "key non-existent"
 | otherwise = (take pos ht) ++ [(0, 0)] ++ (drop (pos + 1) ht)
 where pos = linearProbe ht kk 0 

{- devolvem True ou False, dependendo se uma chave está ou não na tabela -}
hasKey :: HashTable -> Int -> Bool
hasKey [] _ = False
hasKey ((k, v):xs) kk
 | k == kk = True
 | otherwise = hasKey xs kk

linearProbe :: HashTable -> Int -> Int -> Int
linearProbe ht kk ss
 | fst (ht!!(pos)) == kk = pos
 | otherwise = linearProbe ht kk (ss + 3)
 where pos = mod (kk + ss) (length baseExemplo)

baseExemplo :: HashTable
baseExemplo = [(1, 4), (2, 4), (3, 6), (4, 2), (5, 5), (6, 3), (0, 0), (8, 8), (9, 3), (10, 2)]