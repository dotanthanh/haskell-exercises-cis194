{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser
import StackVM as VM

-- ex 1--

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

-- ex 2 --

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- ex 3 --

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit n = ExprT.Lit n
    mul x y = ExprT.Mul x y
    add x y = ExprT.Add x y

reify :: ExprT -> ExprT
reify = id

-- ex 4 --

newtype MinMax = MinMax Integer deriving (Show, Eq)

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Integer where
    lit n = n
    mul = (+)
    add = (*)

instance Expr Bool where
    lit = (<=0)
    mul = (&&)
    add = (||)

instance Expr MinMax where
    lit = MinMax
    mul (MinMax x) (MinMax y) = MinMax $ min x y
    add (MinMax x) (MinMax y) = MinMax $ max x y 

instance Expr Mod7 where
    lit = Mod7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7

-- ex 5 --

instance Expr VM.Program where
    lit n = [VM.PushI n]
    mul a b = a ++ b ++ [VM.Mul]
    add a b = a ++ b ++ [VM.Add]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

run :: String -> Either String VM.StackVal
run = exec . compile
    where exec Nothing = Left "cannot compile, duh"
          exec (Just p) = VM.stackVM p