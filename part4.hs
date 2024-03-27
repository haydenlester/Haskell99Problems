import Data.Function (on)
import Data.List (group, intersect)

-- Delegate 
-- 6k +/- 1 optimization for primality tests
sixK :: [Int]
sixK  = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])

-- Delegate for 31 and 35
-- Lists all prime factors for n, except n itself
primeFactors :: Int -> [Int]
primeFactors n = filter divisGate $ takeWhile sizeGate sixK
  where divisGate x = (n `mod` x == 0)
        sizeGate  x = (x^2 <= n)

-- 31
-- Determine whether a given integer is prime
isPrime :: Int -> Bool
isPrime = null . primeFactors

-- Delegate sequence of all primes
primes = [p | p <- sixK, isPrime p]

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
coprime x y = 1 == gcd x y

-- 34
-- Calculate Euler's totient function
totient :: Int -> Int
totient 1 = 1
totient m = length [x | x <- [1..m-1], (coprime x m)]

-- 35
-- Determine the composed prime factors of n
composePrimeFactors :: Int -> [Int]
composePrimeFactors n = case primeFactors n of
  []    -> [n]
  (p:_) -> p : (composePrimeFactors $ n `div` p)

-- 36
-- Determine the composed prime factors with 
-- their multiplicities, for n.
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map toMult . group . composePrimeFactors
  where toMult xs = (head xs, length xs)

-- 37
-- Calculate Euler's totient function phi(m)
-- Use improved formulaic method
phi :: Int -> Int
phi n = phi' $ primeFactorsMult n
phi' :: [(Int, Int)] -> Int
phi' []           = 1
phi' ((p, m):ns)  = (p - 1) * p ^ (m - 1) * (phi' ns)
