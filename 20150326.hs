{- Defina uma tabela hash em termos de: 
um tipo de dados que representa a tabela propriamente dita
e fuņções get, put, remove e hasKey -}
type HashTable = [(Int, Int)]

{- devolvem um elemento associado a uma chave -}
get :: HashTable -> Int -> Int
get [] kk = -1
get ht kk 
 | hasKey ht kk == False = -1
 | otherwise = snd (linearProbe ht kk 0)

hasKey :: HashTable -> Int -> Bool
hasKey [] _ = False
hasKey ((k, v):xs) kk
 | k == kk = True
 | otherwise = hasKey xs kk

linearProbe :: HashTable -> Int -> Int -> (Int, Int)
linearProbe ht kk ss
 | fst (ht!!(pos)) == kk = ht!!pos
 | otherwise = linearProbe ht kk (ss + 3)
 where pos = mod (kk + ss) (length baseExemplo)


baseExemplo :: HashTable
baseExemplo = [(1, 4), (2, 4), (3, 6), (4, 2), (5, 5), (6, 3), (7, -1), (8, 8), (9, 3), (10, 2)]