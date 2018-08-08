-- Ex. 3
data Stream a = Cons a (Stream a)
              deriving (Show)

streamToList :: Stream a -> [a]
streamToList (Cons a xs) = a : streamToList xs

-- Ex. 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Ex. 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) =
        Cons x (Cons y (interleaveStreams xs ys))

ruler :: Stream Integer
ruler = toStream $ [0] ++ go [0]
    where go acc = let (x:xs) = reverse acc
                       add = reverse ((x+1):xs)
                   in add ++ go $ acc ++ add
          toStream (y:ys) = Cons y (toStream ys)
