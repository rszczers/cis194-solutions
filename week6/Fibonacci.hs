-- Ex1.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Ex2, Binet formula
fib' :: Integer -> Integer
fib' n = round $ phi ** fromIntegral n / sq5
    where
       sq5 = sqrt 5 :: Double
       phi = (1 + sq5) / 2

fibs2 :: [Integer]
fibs2 = map fib' [1..]

fib2' :: [Integer]
fib2' = fib'' 0 1
    where fib'' a b = a : (fib'' b (a+b))
