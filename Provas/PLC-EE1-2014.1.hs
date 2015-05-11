passeioCavalo :: (Int, Int) -> Int -> [(Int, Int)]
passeioCavalo (a,b) n
 | n <= 5 || n `mod` 2 /= 0 = []
 | otherwise = (a,b) : (passeioCavaloAux (a,b) initial)
 where
        initial = [(x,y) | x <- [1..n], y <- [1..n], (x <= n && y <= n) && (x /= a || y /= b)]
 
passeioCavaloAux :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
passeioCavaloAux _ [] = []
passeioCavaloAux (a, b) xs = chosen : passeioCavaloAux chosen rest
 where
        rest = filter (/= chosen) xs -- removes the chosen square
        chosen = snd (minimum numMoves) -- chooses the square from which the knight will have the fewest onward moves
        numMoves = map (\x -> ((length (filter (isMove x) xs)), x)) choices -- maps the number of valid moves from which the knight can go from the given square
        choices = filter (isMove (a,b)) xs -- lists the possible choices from which the knight can go
 
isMove :: (Int, Int) -> (Int, Int) -> Bool
isMove (a, b) (x, y) =
        (a + 2 == x && b + 1 == y)
        || (a + 2 == x && b - 1 == y)
        || (a - 2 == x && b + 1 == y)
        || (a - 2 == x && b - 1 == y)
        || (a + 1 == x && b + 2 == y)
        || (a + 1 == x && b - 2 == y)
        || (a - 1 == x && b + 2 == y)
        || (a - 1 == x && b - 2 == y)

data T a = F | R a (T a) (T a)

fff :: (Show g) => (T b) -> ((g -> String) -> (T b) -> (T String)) -> ((String -> String -> String) -> (T String) -> [a1] -> String) -> String
fff F _ _ = []
fff x m n = n (***) (m show x) []

ggg :: (c -> d) -> (T c) -> (T d)
ggg _ F = F
ggg i ( R a e d) = R ( i a) (ggg i e) ( ggg i d)

hhh :: (Eq (T String)) => (String -> String -> String) -> (T String) -> String -> String
hhh j F l = l
hhh j ( R a e d) l
 | ( R a e d) == ( R " " F F) = "" ++ ( hhh j e l) `j` ( hhh j d l)
 | otherwise = a `j` ( hhh j e l) `j` ( hhh j d l)

(***) :: String -> String -> String
(***) a b = a ++ " " ++ b

result :: (Eq (T String)) => String
result = fff ( R 1 ( R 2 F F) ( R 3 F F) ) ggg hhh

-- FODA-SE ISSO COMPILA