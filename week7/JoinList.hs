import Data.Monoid
import Sized

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
indexJ 1 (Single _ a) = Just a
indexJ 1 (Append _ (Single _ a) _) = Just a
indexJ 2 (Append _ _ (Single _ b)) = Just b
indexJ n t@(Append rS l r)
    | n > jlSize t = Nothing
    | n <= jlSize l = indexJ n l
    | otherwise = indexJ (n - jlSize l) r 
    where jlSize (Append m _ _) = getSize $ size m
          jlSize Empty = 0
          jlSize (Single _ _) = 1

