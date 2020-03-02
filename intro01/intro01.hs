toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
   | length xs `mod` 2 == 0 = x : doubleEveryOther xs
   | otherwise = 2*x : doubleEveryOther xs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n:[]) = sum (toDigits n)
sumDigits (x:xs) = sumDigits [x] + sumDigits xs


validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0


type Peg = Char
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ ((a, b) : hanoi (n-1) c b a)
