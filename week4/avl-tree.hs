data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
    foldr f b Leaf = b
    foldr f b (Node l v r) = f v (foldr f (foldr f b r) l)
    length Leaf = 0
    length (Node l _ r) = 1 + (length l) + (length r)

dswBalance :: Tree a -> Tree a -- DSW algorithm
dswBalance tree = until isBalanced rotate list
    where list = toList tree
          rotate Leaf = Leaf
          rotate t@(Node Leaf _ Leaf) = t
          rotate (Node l1 v1 (Node l2 v2 r2)) = 
                Node (Node l1 v1 l2) v2 (rotate r2)

dswBalance' :: Tree a -> Tree a -- DSW algorithm
dswBalance' tree = until isBalanced rotate list
    where list = toList tree
          rotate Leaf = Leaf
          rotate t@(Node _ _ Leaf) = t
          rotate (Node l1 v1 (Node l2 v2 r2)) = 
                Node (Node l1 v1 l2) v2 (rotate r2)

toList :: Tree a  -> Tree a
toList Leaf = Leaf
toList t@(Node Leaf _ Leaf) = t
toList (Node Leaf v r) = Node Leaf v (toList r)
toList tree = (toList . rotateR) tree

height :: Tree a -> Integer
height Leaf = 0
height (Node l _ r) = 1 + max (height l) (height r)

isBalanced :: Tree a -> Bool
isBalanced t = ((abs . balance) t) < 2

balance :: Tree a -> Integer
balance Leaf = 0
balance (Node l _ r) = (height l) - (height r)

rotateR :: Tree a -> Tree a
rotateR tree
    | (Node (Node a x b) y c) <- tree =
            Node a x (Node b y c)
    | otherwise = tree

rotateL :: Tree a -> Tree a
rotateL tree
    | (Node a x (Node b y c)) <- tree = Node (Node a x b) y c
    | otherwise = tree
