module Party where

import Employee
import Data.Tree

-- Ex. 1
glCons :: Employee -> GuestList -> GuestList
glCons e l@(GL es f) = GL (e:es) (empFun e + f)

instance Monoid GuestList where
        mempty = GL [] 0
        mappend (GL a f1) (GL b f2) = GL (a ++ b) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1 l2 | l1 < l2   = l2
              | otherwise = l1

-- Ex. 2
--data Tree a = Node {
--      rootLabel :: a, -- label value
--      subForest :: [Tree a] -- zero or more child trees
--}
--      p
--      a
--    g/ \g
--    /   \
--   a1   a2
--  g|
--   |
--  a11
--

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold g b (Node a []) = g a b
treeFold g b (Node a fs) = g a $ foldr (flip (treeFold g)) b fs

treeFold' :: (b -> a -> b) -> b -> Tree a -> b
treeFold' g b (Node a []) = g b a
treeFold' g b (Node a fs) = g (foldl (treeFold' g) b fs) a

test1 = (Node 2 [(Node 5 [(Node 6 []), (Node 7 [])]), (Node 8 [])])
test2 = Node "Jeden" [(Node "Dwa" [Node "Osiemnaście" [], Node "Dziewiętnaście" []]), (Node "Trzy" [])]



