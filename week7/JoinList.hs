module JoinList where

import Data.Monoid
import Sized
import Scrabble

-- Ex. 1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

-- Ex. 2
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single m a) = Just a
indexJ _ (Single m a) = Nothing
indexJ i t@(Append m l r)
    | i > jlSize t = Nothing
    | i < jlSize l = indexJ i l
    | otherwise    = indexJ (i - jlSize l) r
    where jlSize (Append m _ _) = getSize $ size m
          jlSize Empty = 0
          jlSize (Single _ _) = 1

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single m a) = Empty
dropJ n t@(Append m l r)
    | n > jlSize t = Empty
    | n < jlSize l = (dropJ n l) +++ r
    | otherwise    = dropJ (n - jlSize l) r
    where jlSize (Append m _ _) = getSize $ size m
          jlSize Empty = 0
          jlSize (Single _ _) = 1

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 jl = Empty
takeJ _ (Single m a) = Single m a
takeJ n t@(Append m l r)
    | n >= jlSize t = t
    | n <= jlSize l = takeJ n l
    | n > jlSize l  = l +++ (takeJ (n - jlSize l) r)
    where jlSize (Append m _ _) = getSize $ size m
          jlSize Empty = 0
          jlSize (Single _ _) = 1

test = Append (Size 7) (Append (Size 6) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')

-- Ex 3. Part II
scoreLine :: String -> JoinList Score String
scoreLine s = foldr (\x xs -> Append (sL (words s))
              (Single (scoreString x) x) xs) Empty (words s)
    where sL = foldr (\s ss -> scoreString s <> ss) (Score 0)
