compose :: (t -> t) -> [(t -> t)] -> [(t -> t)]
compose f [] = []
compose f (g:gs) = [fog] ++ (compose f gs)
 where fog x = f (g x)

data Tree t = 	NilT
				| Node t (Tree t) (Tree t)
				deriving (Eq, Show)

-- Dada uma árvore, gera uma floresta em
-- que todos os nós obedecem o predicado.
filterNodes :: (t -> Bool) -> Tree t -> Tree t
filterNodes f NilT = NilT
filterNodes f (Node n l r)
 | f n = (Node n (filterNodes f l) (filterNodes f r))
 | otherwise = NilT

-- Encontra uma subárvore cuja raiz não obedece ao
-- predicado, e aplica filterTree nas subárvores da mesma.
findNextForest :: (t -> Bool) -> Tree t -> [Tree t]
findNextForest f NilT = []
findNextForest f (Node n l r)
 | f n = (findNextForest f l) ++ (findNextForest f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

-- Dada uma árvore, verifica se a raiz obedece ao predicado.
-- Caso obedeça, gera a floresta desta, e concatena o resultado
-- com as florestas que possam existir nas subárvores à esquerda
-- e à direita (subárvores de uma árvore cuja raiz não obedece ao predicado).
-- Caso contrário, chama a função para as subárvores à esquerda e à direita. 
filterTree :: (t -> Bool) -> Tree t -> [Tree t]
filterTree f NilT = []
filterTree f t@(Node n l r)
 | f n = [filterNodes f t] ++ (findNextForest f l) ++ (findNextForest f r)
 | otherwise = (filterTree f l) ++ (filterTree f r)

testTree :: Tree Int
testTree = (Node 5 (Node 7 (Node 15 NilT (Node 6 NilT NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT))