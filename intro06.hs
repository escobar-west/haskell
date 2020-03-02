import Data.List (foldl')
-- getFib x0 x1 n_outputs
getFib :: Integer -> Integer -> Integer -> Integer
getFib a _ 0 = a
getFib _ b 1 = b
getFib a b n = getFib b (a+b) (n-1)


fibs2 = getFib 0 1 <$> [1..]
