import Control.Monad
import System.IO
import Data.Char (isLetter, isSpace, toUpper)
import Data.Maybe (catMaybes)

{---------------------------------------- TRABALHO 11 ----------------------------------------}
discard :: String -> Maybe String
discard [] = Just []
discard (a:as) = do
	x <- if (isLetter a || isSpace a) then Just a else Nothing
	y <- discard as

	Just ([x] ++ y)

toUpperCase :: Maybe String -> Maybe String
toUpperCase Nothing = Nothing
toUpperCase (Just str) = Just (map toUpper str)

getWord :: String -> String
getWord str = takeWhile (/= ' ') str

dropWord :: String -> String
dropWord str = dropWhile (/= ' ') str

dropSpace :: String -> String
dropSpace str = dropWhile (== ' ') str

splitWords :: String -> [String]
splitWords [] = []
splitWords str = [getWord str] ++ splitWords (dropSpace((dropWord str)))

split :: Maybe String -> [String]
split Nothing = []
split (Just str) = splitWords str

main :: IO()
main = do
	line <- getLine
	result <- return (split (return (return line >>= discard) >>= toUpperCase))
	mapM_ putStrLn result
	main

{---------------------------------------- EXERCÍCIOS ----------------------------------------}
type Stack = [Int]
type Queue = [Int]

{- Questão 3 -}
newtype StateTransformer a = State {runState :: (Stack, Queue) -> (a, (Stack, Queue))}

instance Monad StateTransformer where
	return x = State (\(st, qu) -> (x, (st, qu)))
	(>>=) (State rS) (f) = State $ 
	 \(st, qu) -> let
	 (x, (newStack, newQueue)) = rS (st, qu)
	 (State newRS) = f x
	 in newRS (newStack, newQueue)

pop :: StateTransformer Int
pop = State (\(st, qu) -> if st == [] then error "stack is empty" else (head st, (tail st, qu)))

push :: Int -> StateTransformer ()
push v = State (\(st, qu) -> ((), (v:st, qu)))

insert :: Int -> StateTransformer ()
insert v = State (\(st, qu) -> ((), (st, v:qu)))

remove :: StateTransformer Int
remove = State (\(st, qu) -> if qu == [] then error "queue is empty" else (last qu, (st, init qu)))

{- Questão 2 -}
newtype State stateType a = State {runState :: stateType -> (a, stateType)}

instance Monad (State stateType) where
	return x = State (\stateType -> (x, stateType))
	(>>=) (State rS) (f) = State $ 
	 \stateType -> let
	 (x, newState) = rS newState
	 (State newRS) = f x
	 in newRS newState