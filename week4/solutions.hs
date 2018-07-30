-- Ex1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2 ) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
    where f n | even n = n `div` 2
              | otherwise = 3 * n + 1

-- Ex2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

instance Foldable Tree where
    foldr f b Leaf = b
    foldr f b (Node h l v r) = f v (foldr f (foldr f b r) l)

foldTree :: [a] -> Tree a
foldTree = until isBalanced rotate . toBranch
    where
        rotate Leaf = Leaf
        rotate t@(Node _ _ _ Leaf) = t
        rotate (Node h1 l1 v1 (Node h2 l2 v2 r2)) =
            Node (h1-1) (Node (max (height l1) (height l2)) l1 v1 l2) v2 (rotate r2)
        toBranch [] = Leaf
        toBranch t@(x:xs) = Node (toInteger $ length t) Leaf x (toBranch xs)
        isBalanced t = ((abs . balance) t) < 2
        balance Leaf = 0
        balance (Node _ l _ r) = (height r) - (height l)
        height Leaf = 0
        height (Node _ l _ r) = 1 + max (height l) (height r)

--Ex3.1
xor :: [Bool] -> Bool
xor = foldr (\p q -> (p && (not q)) || ((not p) && q)) False

--Ex3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

--Ex3.3
--foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
--foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

--Ex4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*k + 1 | k <- [1..n], not $ k `elem` m]
    where m = [i + j + 2*i*j | i <- [1..n], j <- [i..n],
                               i + j+2*i*j <= n]
