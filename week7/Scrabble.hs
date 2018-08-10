module Scrabble where

import Data.Monoid
import Data.Char

-- Ex. 3

data Score = Score Int
           deriving (Show, Ord, Eq)

instance Monoid Score where
        mempty = Score 0
        mappend (Score a) (Score b) = Score (a + b)

score :: Char -> Score
score c
    | x `elem` "aeilnorstu" = Score 1
    | x `elem` "dg"         = Score 2
    | x `elem` "bcmp"       = Score 3
    | x `elem` "fhvwy"      = Score 4
    | x == 'k'              = Score 5
    | x `elem` "jk"         = Score 8
    | x `elem` "qz"         = Score 10
    where x = toLower c

scoreString :: String -> Score
scoreString = foldr (\x y -> score x <> y) mempty
