{- TRABALHO 2 -}

-- MERGESORT -----------------------------------------------
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

{- EXERCÍCIOS -}

{- Defina a função menorMaior que recebe
três inteiros e retorna uma tupla com o
menor e o maior deles -}
menorMaior :: (Ord a) => a -> a -> a -> (a, a)
menorMaior a b c = (head x, last x)
 where x = mergesort [a, b, c]

{- Defina a função ordenaTripla que recebe
uma tripla de inteiros e ordena a mesma -}
ordenaTripla :: (Ord a) => (a, a, a) -> (a, a, a)
ordenaTripla (a, b, c) = (x!!0, x!!1, x!!2)
  where x = mergesort [a, b, c]

{- Uma linha pode ser representada da seguinte forma: -}
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

{- Defina funções que retornem: -}
{- a primeira coordenada de um ponto -}
getX :: Ponto -> Float
getX (x, y) = x

{- a segunda coordenada de um ponto -}
getY :: Ponto -> Float
getY (x, y) = y

{- indique se uma reta é vertical ou não -}
isVertical :: Reta -> Bool
isVertical ((x1, y1), (x2, y2))
 | x1 == x2 = True
 | otherwise = False

pontoY :: Float -> Reta -> Float
pontoY x r@((x1, y1), (x2, y2))
 | isVertical r = error "line is vertical"
 | otherwise = ((y2 - y1)/(x2 - x1)) * (x - x1) + y1

{- Funções sobre o banco de dados - consultas -}
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros (x:xs) p
 | fst x == p = (snd x):livros xs p
 | otherwise = livros xs p

{- using list comprehension
livros :: BancoDados -> Pessoa -> [Livro]
livros xs pp = [l | (p, l) <- xs, pp == p]
-}

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos (x:xs) l
 | snd x == l = (fst x):emprestimos xs l
 | otherwise = emprestimos xs l

{- using list comprehension
emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos xs ll = [p | (p, l) <- xs, ll == l]
-}

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado (x:xs) l
 | l == snd x = True
 | otherwise = emprestado xs l

{- using list comprehension
emprestado xs ll = not (null [l | (p, l) <- xs, ll == l])
-}

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos (x:xs) p
 | fst x == p = 1 + qtdEmprestimos xs p
 | otherwise = qtdEmprestimos xs p

{- using list comprehension
qtdEmprestimos xs pp = length [p | (p, l) <- xs, pp == p]
-}

{- Funções sobre o banco de dados - atualizações -}
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = [(p, l)]	
emprestar (x:xs) p l = x:(emprestar xs) p l

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = []
devolver (x:xs) p l
 | fst x == p && snd x == l = devolver xs p l
 | otherwise = x:(devolver xs p l)

{- using list comprehension
devolver xs pp ll = [(p, l) | (p, l) <- xs, pp /= p || ll /= l]
-}

{- function 'membro' using list comprehension -}
membro :: [Int] -> Int -> Bool
membro xs xx = not (null [x | x <- xs, xx == x])

{- Defina uma função que ordena uma lista de
inteiros utilizando o algoritmo quick sort -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (a:as) = quicksort ([x | x <- as, x <= a]) ++ [a] ++ quicksort ([x | x <- as, x > a])

baseExemplo :: BancoDados
baseExemplo = 
	[("Sergio", "O Senhor dos Aneis"),
	("Andre", "Duna"),
	("Fernando", "Jonathan Strange & Mr. Norrell"),
	("Fernando", "A Game of Thrones")]

{- Processamento de Texto -}
getWord :: String -> String
getWord str = takeWhile (/= ' ') str

dropWord :: String -> String
dropWord str = dropWhile (/= ' ') str

dropSpace :: String -> String
dropSpace str = dropWhile (== ' ') str

type Word = String
splitWords :: String -> [Word]
splitWords [] = []
splitWords str = [getWord str] ++ splitWords (dropSpace((dropWord str)))