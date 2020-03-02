{-# OPTIONS_GHC -Wall #-}
module Intro where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
   | even x = (x - 2) * fun1 xs
   | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' xs = let f a b = (a-2) * b
           in foldr f 1 $ filter even xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
   | even n = n + fun2 (n `div` 2)
   | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' n = let collatz z | even z = z `div` 2 | otherwise = 3*z + 1
          in sum $ filter even $ takeWhile (/=1) $ iterate collatz n

isEqual :: Integer -> Bool
isEqual x = fun2 x == fun2' x


data Tree a = Leaf
   | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)


getHeight :: Tree a -> Integer
getHeight (Node x _ _ _) = x
getHeight Leaf = -1


addNode :: a -> Tree a -> Tree a
addNode a Leaf = Node 0 Leaf a Leaf
addNode a (Node x left var right)
   | getHeight left < getHeight right = Node x (addNode a left) var right
   | getHeight left > getHeight right = Node x left var (addNode a right)
   | otherwise = let newTree = addNode a left
                 in Node (1 + getHeight newTree) newTree var right


foldTree :: [a] -> Tree a
foldTree xs = foldr addNode Leaf $ reverse xs

xor' :: Bool -> Bool -> Bool
xor' a b
   | a == b = False
   | otherwise = True


xor :: [Bool] -> Bool
xor = foldr xor' False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x bs -> (f x):bs) []
