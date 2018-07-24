module Golf where

-- Ex1
skips :: [a] -> [[a]]
skips [] = []
skips s = [skip n s | n <- [1..(length s)]]
    where skip n [] = []
          skip n s = (take 1 (drop (n-1) s)) ++ (skip n (drop n s))

-- Ex2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima t@[x] = t
localMaxima t@[x,y] = t
localMaxima (x:y:z:zs)
    | y > x && y > z = (y : localMaxima zs)
    | otherwise = localMaxima (y:z:zs)

-- Ex3
-- Histogram rysuje się od góry zaznaczając elementy maksymalne;
-- w kolejnym kroku wywołuje się funkcję dla listy o zredukowanej
-- o jeden liczby wystąpień elementów maksymalnych
histogram :: [Integer] -> String
histogram [] = []
histogram t = go height xy
     where xy = asoCount t
           height  = maximum [y | (x, y) <- xy]
           go 0 _ = "==========\n0123456789\n"
           go h k = typeLine k ++ "\n" ++ go (h-1) (decreaseMax k)

-- Generuje linię zaznaczając przez '*' elementy maksymalne           
typeLine :: [(Integer, Integer)] -> String
typeLine t = map f t
    where f (x, y) | y == maximum [b | (a, b) <- t] = '*'
                   | otherwise = ' '
                                          
-- Zmniejsza o jeden liczbę wystąpień elementów maksymalnych
decreaseMax :: [(Integer, Integer)] -> [(Integer, Integer)]
decreaseMax t = go (maximal t) t
    where go _ [] = []
          go t ((x,y):xs)
            | (x, y) `elem` t = [(x, (y-1))] ++ go t xs
            | otherwise = [(x, y)] ++ go t xs

-- Zwraca pary, których pierwsza wartośc występuje najczęściej na liście
maximal :: [(Integer, Integer)] -> [(Integer, Integer)]
maximal ls = [(x, y) | (x, y) <- ls, y == maximum [n | (z, n) <- ls]]

-- Robi listę (element, liczba wystąpień)
asoCount :: [Integer] -> [(Integer, Integer)]
asoCount ls = [(x, countEl x ls) | x <- [0..9]]

-- Liczy liczbę wystąpień elementu e na liście
countEl :: (Eq a) => a -> [a] -> Integer
countEl e [] = 0
countEl e (x:xs) 
    | e == x = 1 + countEl e xs
    | otherwise = countEl e xs
