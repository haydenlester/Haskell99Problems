-- 31
-- Determine whether a given integer is prime
-- Using 6k +/- 1 optimization
-- This impl is mostly just an experiment in list comprehensions
-- It is not readable.
isPrime :: Int -> Bool
isPrime n = [] == [p | p <- takeWhile ((<= n) . (^2)) sixK, n `mod` p == 0]
  where sixK  = [2, 3, 5] ++ [6*k + i | k <- [1..], i <- [1, 5]]

-- more readable equivalent version
-- see wikipedia on primality testing
isPrime' :: Int -> Bool
isPrime' n = [] == takeWhile (\p -> p*p <= n) valids
  where sixK    = [2, 3, 5] ++ [6*k + i | k <- [1..], i <- [1, 5]]
        valids  = [p | p <- sixK, n `mod` p == 0]
