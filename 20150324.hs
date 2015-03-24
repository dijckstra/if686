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

baseExemplo :: BancoDados
baseExemplo = 
	[("Sergio", "O Senhor dos Aneis"),
	("Andre", "Duna"),
	("Fernando", "Jonathan Strange & Mr. Norrell"),
	("Fernando", "A Game of Thrones")]