import Data.Char (ord)

{- TRABALHO 6 -}
type Node a = a
type Edge a = (Node a, Node a, Int)

data Graph a =	Empty 
				| Graph [Node a] [Edge a]
				deriving (Eq, Ord, Show)

testGraph :: Graph Int
testGraph = Graph [1, 2, 3] [(1, 2, 1), (1, 3, 1), (2, 3, 1)]

{- EXERCÍCIOS -}
{- Defina, usando map, uma função que calcula a raiz
quadrada de todos os elementos de uma lista de números -}
sqrtList :: [Float] -> [Float]
sqrtList xs = map sqrt xs

{- Defina uma função posicaoAlfabeto que, dado
um String, devolve uma lista contendo as posições
de cada caractere dessa String no alfabeto -}
reducing :: Int -> Int
reducing c = c - 96

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto xs = map reducing ascii
 where
 	ascii = (map ord xs)

{- Defina map usando compreensões de listas -}
mapped :: (t -> u) -> [t] -> [u]
mapped f a = [f x | x <- a]

{- member usando foldr -}
member :: (Eq t) => t -> [t] -> Bool
member n xs = foldr (||) (False) (map (== n) xs)

unite :: (Eq t) => t -> [t] -> [t]
unite a b
 | member a b = b
 | otherwise = [a] ++ b

union :: (Eq t) => [t] -> [t] -> [t]
union a b = foldr (unite) [] (a ++ b)

soma :: String -> Int
soma  l = foldr (+) 0 (posicaoAlfabeto l)

somaCaracteres :: [String] -> [Int]
somaCaracteres l = map soma l

data Tree t = 	NilT
				| Node t (Tree t) (Tree t)
				deriving (Eq, Show)

depth :: Tree t -> Int
depth (NilT) = -1
depth (Node _ l r) = 1 + max (depth l) (depth r)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node t l r) = (collapse l) ++ [t] ++ (collapse r) -- In-order traversal

insert :: t -> Tree t -> Tree t
insert n NilT = (Node n (NilT) (NilT))
insert n (Node a l r)
 | depth l < depth r = Node a (insert n l) (r)
 | otherwise = Node a (l) (insert n r)

criarArvore :: (Ord t) => [t] -> (t -> Tree t -> Tree t) -> Tree t
criarArvore xs f = foldr f NilT xs

testTree :: Tree Char
testTree = (Node 'F' (Node 'B' (Node 'A' (NilT) (NilT)) (Node 'D' (Node 'C' (NilT) (NilT)) (Node 'E' (NilT) (NilT)))) (Node 'G' (NilT) (Node 'I' (Node 'H' (NilT) (NilT)) (NilT))))

nonNegatives :: [Int] -> [Int]
nonNegatives xs = filter (>= 0) xs