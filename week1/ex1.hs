toDigits :: Integer -> [Integer]
toDigits x 
    | x <= 0 =  []
    | x > 0 = go x [] where
        go a b
            | a < 10  = a : b
            | otherwise =
                let r = a `mod` 10
                in go ((a - r) `div` 10) (r : b)

toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = go (reverse x) [] where
    go [x] [] = [x]
    go [x] b = x:b
    go [] b = b
    go (x1:x2:xs) b = go xs ((2*x2):x1:b)

sumDigits :: [Integer] -> Integer
sumDigits x = foldl1 (+) [z | w1 <- x, z <- (toDigits w1)]

validate :: Integer -> Bool
validate x
    | f x `mod` 10 == 0 = True
    | otherwise = False
        where f = sumDigits . doubleEveryOther. toDigits
