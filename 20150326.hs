{-------------------------------- TRABALHO 3 --------------------------------}

{- Defina uma tabela hash em termos de: 
um tipo de dados que representa a tabela propriamente dita
e fuņções get, put, remove e hasKey -}

{- Nessa tabela, um lugar vazio é definido por (0, 0). -}

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
 | full ht = error "table is full"
 | hasKey ht kk == False = (take emptyPos ht) ++ [(kk, vv)] ++ (drop (emptyPos + 1) ht)
 | otherwise = (take pos ht) ++ [(kk, vv)] ++ (drop (pos + 1) ht)
 where 
 	emptyPos = linearProbe ht 0 0
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

{-Defina uma funcao comparaConjuntos :: (Eq t) => [t] ->[t]->String 
que responda se o primeiro conjunto A contem o segundo conjunto B,
se B contem A, se ha intersecao entre eles, se eles sao disjuntos ou se eles sao
iguais. Caso A contenha B, a sa ́ıda deve ser “A contem B”; caso B contenha
A, a saıda deve ser “B contem A”; caso haja intersecao, mas nenhum conjunto
contenha o outro, a sa ́ıda deve ser “A interseciona B”; caso nao haja nenhum
elemento em comum, a sa ́ıda deve “Conjuntos disjuntos”; caso os conjuntos
sejam iguais, a saıda deve ser “A igual a B".-}

existeElemento :: (Eq a) => [a]-> a-> Bool
existeElemento list x 
		|list == [] = False
		|(head list) == x = True
		|otherwise = existeElemento (tail list) x

contem :: (Eq a) => [a]-> [a]-> Bool
contem x y 
		|x == [] = True
		|otherwise = existeElemento y (head x) && contem (tail x) y
		
intersecao :: (Eq a) => [a]-> [a] ->Bool
intersecao x y 
		|(x == [])= False
		|existeElemento y (head x) = True
		|otherwise =  intersecao y (tail x)

comparaConjuntos :: (Eq t) => [t] -> [t] -> String
comparaConjuntos a b
		|(contem a b && contem b a) = "A igual a B"
		|contem a b = "B contem A"
		|contem b a = "A contem B"
		|intersecao a b = "A interseciona B"
		|otherwise = "conjuntos disjuntos"

{-------------------------------- EXERCÍCIOS --------------------------------}

-- Funções polimórficas
took :: Int -> [a] -> [a]
took _ [] = []
took 0 xs = []
took n (x:xs) = x:(took (n - 1) xs)

dropped :: Int -> [a] -> [a]
dropped _ [] = []
dropped 0 xs = xs
dropped n (x:xs) = dropped (n - 1) xs

tookWhile :: (a -> Bool) -> [a] -> [a]
tookWhile _ [] = []
tookWhile f (x:xs)
 | f x = x:(tookWhile f xs)
 | otherwise = []

droppedWhile :: (a -> Bool) -> [a] -> [a]
droppedWhile _ [] = []
droppedWhile f a@(x:xs)
 | f x = droppedWhile f xs
 |otherwise = a

{- Defina uma função polimórfica que ordena
uma lista de valores para os quais os
operadores de comparação ((>), (>=), etc.)
estão definidos. -}
{- MERGESORT DO TRABALHO 2 -}

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 | x < y = x:(merge xs (y:ys))
 | otherwise = y:(merge (x:xs) ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort first) (mergesort second)
 where
 	(first, second) = splitAt (((length xs) + 1) `div` 2) xs

{- Crie uma função agrupar que recebe uma lista de listas de
valores de um tipo t que podem ser comparados para saber se
são iguais e devolve uma lista de pares (t, Int) onde o primeiro
elemento é um valor do tipo t que existe em pelo menos uma
das sub-listas da entrada e o segundo é o número de ocorrências
desse valor nas sub-listas -}

agrupar :: (Eq t) => [[t]] -> [(t, Int)]
agrupar x = count (concat x)

count :: (Eq t) => [t] -> [(t, Int)]
count [] = []
count l@(x:xs) = [(x, length ([a | a <- l, a == x]))] ++ count [a | a <- l, a /= x]