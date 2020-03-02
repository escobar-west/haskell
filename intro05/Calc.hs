{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as SVM

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


evalStr :: String -> Maybe Integer
evalStr s = (parseExp Lit Add Mul) s >>= Just . eval


testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul


testExprT = testExp :: String -> Maybe ExprT
testInteger = testExp :: String -> Maybe Integer
testBool = testExp :: String -> Maybe Bool
testMM = testExp :: String -> Maybe MinMax
testSat = testExp :: String -> Maybe Mod7
testProg = testExp :: String -> Maybe SVM.Program


