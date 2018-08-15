{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Data.Char
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
-- pure    :: Applicative f => a -> f a
-- pure [] :: Applicative f => f [t]
-- <|>     :: Applicative f => f a -> f a -> f a
-- oneOrMore <|> :: Parser [a] -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
-- (:) <$> :: Functor f => f a -> f ([a] -> [a])
-- (:) <$> p :: Parser ([a] -> [a])
-- (:) <$> p <*> :: Parser [a] -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p 

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
-- zeroOrMore :: Parser a -> Parser [a]
-- satisfy isSpace :: Parser Char
-- zeroOrMore $ satisfy isSpace :: Parser [Char]
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
-- (:) <$> :: Functor f => f a -> f [a] -> f [a]
-- satisfy isAlpha                 :: Parser Char 
-- zeroOrMore (satisfy isAlphaNum) :: Parser [Char]
-- (:) <$> satisfy isAlpha :: Parser [Char]
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
