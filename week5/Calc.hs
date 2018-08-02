{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

--Ex. 1
eval :: ExprT -> Integer
eval (ExprT.Lit a) = a
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

--Ex. 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

--Ex. 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit a = ExprT.Lit a
    add a b = ExprT.Add a b
    mul a b = ExprT.Mul a b

--Ex. 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit a = a
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit a = a > 0
    add a b = a || b
    mul a b = a && b

instance Expr MinMax where
    lit a = MinMax a
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
    lit a = Mod7 a
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

--Ex. 5
instance Expr (StackVM.Program) where
    lit a = [StackVM.PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = parseExp lit add mul s :: Maybe StackVM.Program

--Ex. 6

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              deriving (Show, Eq)

instance Expr VarExprT where
    lit n = Calc.Lit n
    add a b = Calc.Add a b
    mul a b = Calc.Mul a b

instance HasVars VarExprT where
    var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = M.lookup s

-- instance Expr (M.Map String Integer -> Maybe Integer) where
--    lit n = \_ -> Just n
--    add a b = \d -> 
