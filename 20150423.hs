import Data.List
{- Quantos itens existem nas seguintes listas?
[2,3] = 2
[[2,3]] = 1

Qual o tipo de [[2,3]]? Lista de lista de inteiros = [[Int]]

Qual o resultado da avaliação de:

[2,4..9] = [2,4,6,8]
[2..2] = [2]
[2,7..4] = [2]
[10,9..1] = [10,9,8,7,6,5,4,3,2,1]
[10..1] = []
[2,9,8..1] = erro de compilação -}

{- Implemente uma função que, dado um número inteiro
N, retorne uma lista de inteiros com os N primeiros
números pares da sequência de Fibonacci. -}
fibonacci :: [Int]
fibonacci = 0 : 1 : next fibonacci
 where
 	next (a : t@(b:_)) = (a+b) : next t

evenFibs :: Int -> [Int]
evenFibs n = take n (filter (\n -> n `mod` 2 == 0) fibonacci)

{- Crie um função que recebe uma lista de
inteiros e retorna a lista ordenada em função
da soma de seus digitos (crescente) -}
ordenar :: [Int] -> [Int]
ordenar xs = [snd x | x <- sort (map (digitalRoot) xs)]

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

digitalRoot :: Int -> (Int, Int)
digitalRoot n
 | n < 10 = (n, n)
 | otherwise = (fst (digitalRoot (sum (digs n))), n)

{- Processamento de Texto -}
type Word = String
type Line = [Word]

{- Questões já feitas (ver 20150324.hs). -}
getWord :: String -> String
getWord str = takeWhile (/= ' ') str

dropWord :: String -> String
dropWord str = dropWhile (/= ' ') str

dropSpace :: String -> String
dropSpace str = dropWhile (== ' ') str

splitWords :: String -> [Word]
splitWords [] = []
splitWords str = [getWord str] ++ splitWords (dropSpace((dropWord str)))

{- Questões não feitas -}
gotLine :: Int -> [Word] -> Line
gotLine _ [] = []
gotLine n (x:xs)
 | n < length x = []
 | otherwise = x : (gotLine (n - (length x)) (xs))

dropLine :: Int -> [Word] -> Line
dropLine _ [] = []
dropLine n a@(x:xs)
 | n < length x = a
 | otherwise = dropLine (n - (length x)) (xs)

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines xs = (gotLine 10 xs) : splitLines (dropLine 10 xs)

fill :: String -> [Line]
fill st = splitLines (splitWords st)

joinLine :: Line -> String
joinLine [] = []
joinLine (x:xs) = x ++ " " ++ joinLine xs

joinLines :: [Line] -> String
joinLines [] = []
joinLines (x:xs) = joinLine x ++ joinLines xs