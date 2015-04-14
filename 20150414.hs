compose :: (t -> t) -> [(t -> t)] -> [(t -> t)]
compose f [] = []
compose f (g:gs) = [fog] ++ (compose f gs)
 where fog x = f (g x)

type GNode a = a
type Edge a = (GNode a, GNode a, Int)

data Graph a = Graph [GNode a] [Edge a] deriving (Eq, Ord, Show)

mapGraph :: (t -> u) -> (Graph t) -> (Graph u)	
mapGraph f (Graph ns es) = Graph [f n | n <- ns] [((f a), (f b), c) | (a, b, c) <- es]

foldGraph :: (t -> t -> t) -> Graph t -> t
foldGraph f (Graph [] []) = error "graph is empty"
foldGraph f (Graph ns _) = foldr1 f ns

testGraph :: Graph Int
testGraph = Graph [6, 4, 3, 5, 2, 1] [(6, 4, 3), (3, 4, 1), (4, 5, 2), (2, 3, 5), (5, 2, 10), (5, 1, 8), (2, 1, 9)]

data Tree t = NilT | TNode t (Tree t) (Tree t) deriving (Eq, Show)

-- Dada uma árvore, gera uma floresta em
-- que todos os nós obedecem o predicado.
filterNodes :: (t -> Bool) -> Tree t -> Tree t
filterNodes f NilT = NilT
filterNodes f (TNode n l r)
 | f n = (TNode n (filterNodes f l) (filterNodes f r))
 | otherwise = NilT

-- Encontra uma subárvore cuja raiz não obedece ao
-- predicado, e aplica filterTree nas subárvores da mesma.
findNextForest :: (t -> Bool) -> Tree t -> [Tree t]
findNextForest f NilT = []
findNextForest f (TNode n l r)
 | f n = (findNextForest f l) ++ (findNextForest f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

-- Dada uma árvore, verifica se a raiz obedece ao predicado.
-- Caso obedeça, gera a floresta desta, e concatena o resultado
-- com as florestas que possam existir nas subárvores à esquerda
-- e à direita (subárvores de uma árvore cuja raiz não obedece ao predicado).
-- Caso contrário, chama a função para as subárvores à esquerda e à direita. 
filterTree :: (t -> Bool) -> Tree t -> [Tree t]
filterTree f NilT = []
filterTree f t@(TNode n l r)
 | f n = [filterNodes f t] ++ (findNextForest f l) ++ (findNextForest f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

testTree :: Tree Int
testTree = (TNode 5 (TNode 7 (TNode 15 NilT (TNode 6 NilT NilT)) (TNode 2 NilT NilT)) (TNode 10 NilT NilT))