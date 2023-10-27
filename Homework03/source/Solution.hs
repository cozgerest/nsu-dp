module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = if x `elem` xs then False else unique xs


pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]


primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, (gcd a b `gcd` c) == 1]

divisors :: Integral a => a -> [a]
divisors n = [d | d <- [1..n `div` 2], n `mod` d == 0]
perfectNumbers :: Integral a => [a]
perfectNumbers = [n | n <- [2, 4..], n == sum (divisors n)]

triangularNumber :: Integral a => a -> a
triangularNumber k = k * (k + 1) `div` 2
-- https://math.stackexchange.com/questions/222709/inverting-the-cantor-pairing-function
cantorPairs :: Integral a => [(a, a)]
cantorPairs = [(x, y) | i <- [0..], let n = floor (sqrt (8 * fromIntegral i + 1) - 1) `div` 2, let y = i - triangularNumber n, let x = n - y]


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
