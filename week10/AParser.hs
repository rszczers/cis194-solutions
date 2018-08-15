{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
-- Ex. 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

--newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
instance Functor Parser where
        fmap g (Parser f) = Parser $ (first g <$>) . f
        -- first g :: (a, c) -> (b, c)
        -- first g <$> :: (Functor f) => f (a, c) -> f (b, c)
        -- first g <$> . f :: String -> Maybe (b, String)

-- Ex. 2

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    -- g :: String -> Maybe (a -> b, String)
    -- f :: String -> Maybe (a, String)
    (Parser g) <*> (Parser f) = Parser $ \s -> case g s of
        Nothing       -> Nothing
        -- f :: String -> Maybe (a, String)
        -- first g' <$> :: Functor f => f (a, c) -> f (b, c)
        -- f s' :: Maybe (a, String)
        Just (g', s') -> first g' <$> f s'

-- type Name = String
-- data Employee = Emp { name :: Name, phone :: String }

-- parseName :: Parser Name
-- parsePhone :: Parser String

-- Emp <$> parseName <*> parsePhone :: Parser Employee
-- Emp :: Name -> String -> Employee
-- Emp <$> Functor f => f Name -> f (String -> Employee)
-- Emp <$> parseName :: Parser (String -> Employee)
-- Emp <$> parseName <*> :: Parser String -> Parser Employee
-- Emp <$> parseName <*> parsePhone :: Parser Employee

-- Ex. 3
abParser :: Parser (Char, Char)
-- ((,) <$> :: Functor f => f a -> f (b -> (a, b))
-- ((,) <$> char 'a') :: Parser (b -> (Char, b))
-- ((,) <$> char 'a' <*>) :: Parser b -> Parser (Char, b)
-- ((,) <$> char 'a' <*> char 'b') :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
-- \a b -> () :: a -> b -> ()
-- \a b -> () <$> :: Functor f => f a -> f (b -> ())
-- \a b -> () <$> char 'a' :: Parser (b -> ())
-- \a b -> () <$> char 'a' <*> :: Parser b -> Parser ()
abParser_ = (\a b -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
-- \x y -> [x, y] :: a -> a -> [a]
-- \x y -> [x, y] <$> :: Functor f -> fa -> f (a -> [a])
-- posInt :: Parser Integer
-- \x y -> [x, y] <$> posInt :: Parser (Integer -> [Integer])
-- \x y -> [x, y] <$> posInt <*> :: Parser Integer -> Parser [Integer]
-- \x y -> [x, y] <$> posInt <*> posInt :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

-- Ex. 4
-- class Applicative f => Alternative f where
--      empty :: f a
--      (<|>) :: f a -> f a -> f a

instance Alternative Parser where
-- Parser :: (String -> Maybe (a, String)) -> Parser a
-- const Nothing :: b -> Maybe a
-- Parser $ const Nothing = Parser a
    empty = Parser $ const Nothing
    (Parser g) <|> (Parser f) = Parser $ \s -> (g s <|> f s)

-- parses either an integer value or an uppercase character, and
-- fails otherwise.
intOrUppercase :: Parser ()
-- postInt :: Parser Integer
-- satisfy (isUpper) :: Parser Char
-- \x -> () :: a -> ()
-- \x -> () <$> :: Functor f => f a -> f () 
-- (\x -> ()) <$> postInt :: Parser ()
-- (\x -> ()) <$> satisfy (isUpper) :: Parser ()
intOrUppercase =
    ((\x -> ()) <$> posInt) <|> ((\x -> ()) <$> satisfy (isUpper))

