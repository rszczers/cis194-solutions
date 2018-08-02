{-# LANGUAGE TypeSynonymInstances #-}

import Parser
import StackVM

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

--Ex. 5
instance Expr Program where
	lit a = [PushI a]
	add a b = a ++ b ++ [Add]
	mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s :: Maybe Program
