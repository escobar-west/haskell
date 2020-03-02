{-# OPTIONS_GHC -Wall #-}
module Golf where


nSkips :: [a] -> Int -> [a]
nSkips xs n
   | n <= 1 = xs
   | otherwise = map fst $ filter (\z -> (snd z) `mod` n == 0) $ zip xs [1..]

skips :: [a] -> [[a]]
skips xs = map (nSkips xs) [1..(length xs)]


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
   | a < b && b > c = b : localMaxima (b:c:xs)
   | otherwise = localMaxima (b:c:xs)
localMaxima _ = []
