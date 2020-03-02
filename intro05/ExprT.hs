{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ExprT where

import qualified StackVM as SVM

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a


data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)


instance Expr ExprT where
   lit x = Lit x
   add a b = Add a b
   mul a b = Mul a b


instance Expr Integer where
   lit x = x
   add a b = a + b
   mul a b = a * b


instance Expr Bool where
   lit x | x > 0 = True | otherwise = False
   add a b = a || b
   mul a b = a && b


instance Expr MinMax where
   lit x = MinMax x
   add (MinMax a) (MinMax b) = MinMax $ max a b
   mul (MinMax a) (MinMax b) = MinMax $ min a b


instance Expr Mod7 where
   lit x = Mod7 (x `mod` 7)
   add (Mod7 a) (Mod7 b) = Mod7 $ (a+b) `mod` 7
   mul (Mod7 a) (Mod7 b) = Mod7 $ (a*b) `mod` 7

instance Expr SVM.Program where
   lit x = [SVM.PushI x]
   add p1 p2 = concat [p1, p2, [SVM.Add]]
   mul p1 p2 = concat [p1, p2, [SVM.Mul]]
