import Data.Function (on)
import Data.List (intersect)

-- delegate for 31 and 35
primeFs :: Int -> [Int]
primeFs n = [p | p <- takeWhile ((<= n) . (^2)) sixK, n `mod` p == 0]
  where sixK  = [2, 3, 5] ++ [6*k + i | k <- [1..], i <- [1, 5]]

-- 31
-- Determine whether a given integer is prime
-- Using 6k +/- 1 optimization
-- (i know its hard to read)
isPrime :: Int -> Bool
isPrime n = [] == primeFs n

-- 32
-- Use Euclid's algorithm to determine GCD of two integers.
gcd' :: Int -> Int -> Int
gcd' a b = case compare a b of
  EQ -> a
  LT -> gcd' a (b-a)
  GT -> gcd' (a-b) b

-- 33
-- Determine whether two positive integers are coprime.
coprime :: Int -> Int -> Bool
coprime x y = [] == intersect (primeFs x) (primeFs y)

-- 34
-- Calculate Euler's totient function
totient :: Int -> Int
totient 1 = 1
totient m = length [x | x <- [1..m-1], coprime x m]
