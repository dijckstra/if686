type Node a = a
type Edge a = (Node a, Node a, Int)

data Graph a =	Empty 
				| Graph [Node a] [Edge a]
				deriving (Eq, Ord, Show)

testGraph :: Graph Int
testGraph = Graph [1, 2, 3] [(1, 2, 1), (1, 3, 1), (2, 3, 1)]