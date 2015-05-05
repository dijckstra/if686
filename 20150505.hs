import Control.Monad
import System.IO
import Data.Char (isLetter, isSpace, toUpper)
import Data.Maybe (catMaybes)

{-------------------- TRABALHO 11 --------------------}
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