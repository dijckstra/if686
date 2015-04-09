data Shape = Circle Float | Rectangle Float Float

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l w) = w * l

data Weekday = 	Sunday
				| Monday Int [String]
				| Tuesday Int [String]
				| Wednesday Int [String]
				| Thursday Int [String]
				| Friday Int [String]
				| Saturday

isWeekend :: Weekday -> Bool
isWeekend (Sunday) = True
isWeekend (Saturday) = True
isWeekend (_) = False

isPLC :: [String] -> Bool
isPLC [] = False
isPLC (a:as)
 | a == "PLC" = True
 | otherwise = isPLC as

hasPLC :: Weekday -> Bool
hasPLC (Sunday) = False
hasPLC (Saturday) = False
hasPLC (Monday h as) = isPLC as
hasPLC (Tuesday h as) = isPLC as
hasPLC (Wednesday h as) = isPLC as
hasPLC (Thursday h as) = isPLC as
hasPLC (Friday h as) = isPLC as

data Expr =	Lit Int
			| Add Expr Expr
			| Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit i) = show i
showExpr (Add a b) = "(" ++ showExpr a ++ " + " ++ showExpr b ++ ")"
showExpr (Sub a b) = "(" ++ showExpr a ++ " - " ++ showExpr b ++ ")"

testExpr :: Expr
testExpr = (Add (Add (Lit 3) (Lit 8)) (Sub (Add (Lit 13) (Lit 83)) (Sub (Lit 12) (Lit 9))))

data List t =	Nil
				| Cons t (List t)

toList :: List t -> [t]
toList Nil = []
toList (Cons c ls) = [c] ++ toList ls

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = (Cons a (fromList as))

testList :: List Int
testList = (Cons 6 (Cons 4 (Cons 2 (Cons 3 (Cons 4 (Nil))))))

data Tree t = 	NilT
				| Node t (Tree t) (Tree t)
				deriving (Eq, Show)

depth :: Tree t -> Int
depth (NilT) = -1
depth (Node _ l r) = 1 + max (depth l) (depth r)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node t l r) = (collapse l) ++ [t] ++ (collapse r) -- In-order traversal

bfs :: (Eq t) => Tree t -> t -> Bool
bfs NilT _ = False
bfs (Node t l r) n
 | t == n = True
 | otherwise = (bfs l n) || (bfs r n)

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node t l r) = Node (f t) (mapTree f l) (mapTree f r)

testTree :: Tree Char
testTree = (Node 'F' (Node 'B' (Node 'A' (NilT) (NilT)) (Node 'D' (Node 'C' (NilT) (NilT)) (Node 'E' (NilT) (NilT)))) (Node 'G' (NilT) (Node 'I' (Node 'H' (NilT) (NilT)) (NilT))))