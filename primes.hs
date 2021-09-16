primes :: [Integer]
primes = sieve [2 ..]
  where sieve (x : xs) = x : sieve [ y | y <- xs, y `mod` x > 0 ]

primeProduct :: Int -> Integer
primeProduct k = 1 + product (take k primes)

conjecture :: Int -> Bool
conjecture k = null [ x | x <- [2 .. factor], prod `mod` toInteger x == 0 ]
 where
  prod   = primeProduct k
  factor = floor . sqrt $ fromIntegral prod

-- This fails for k = 6
-- head $ filter (not . conjecture) [1..] = 6

primePairs :: [(Integer, Integer)]
primePairs = pair primes
 where
  pair (x : y : rest) | x + 2 == y = (x, y) : pair (y : rest)
                      | otherwise  = pair (y : rest)

primeTriples :: [(Integer, Integer, Integer)]
primeTriples = triple primes
 where
  triple (x : y : z : rest)
    | x + 2 == y && y + 2 == z = (x, y, z) : triple (y : z : rest)
    | otherwise                = triple (y : z : rest)

-- Learnings
--
-- 1. Using generator lists (like primes) is very expressive
-- 2. Lazy evaluation in Haskell allows you to express these theorems in an intuitive way
