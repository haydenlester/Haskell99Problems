import Data.Function (on)
import Data.List (group, intersect, find)

-- Delegate 
-- 6k +/- 1 optimization for primality tests
sixK :: [Int]
sixK  = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])

-- Delegate for 31 and 35
-- Lists all prime factors for n, except n itself
primeFactors :: Int -> [Int]
primeFactors n = [x | x <- sixK, n `mod` x == 0]

-- 31
-- Determine whether a given integer is prime
isPrime :: Int -> Bool
isPrime n = null $ takeWhile (\x -> x^2 <= n) $ primeFactors n

-- delegate 
primes :: [Int]
primes = [x | x <- sixK, isPrime x]

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
composePrimeFactors n = case take 1 $ primeFactors n of
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

-- 38
-- Compare the formulaic totient to the other method (no sol)


-- 39
-- Find prime numbers in a range
-- Todo: Use this to check out primorials and 
primesR :: Int -> Int -> [Int]
primesR x y 
  | x `mod` 2 == 0  = filter isPrime [x+1,x+3..y]
  | otherwise       = filter isPrime [x,x+2..y]

-- 40
-- Goldbach conjecture: Every positive even number > 2 
-- is the sum of two prime numbers.
-- This is a bit sluggish, but neat
goldbach :: Int -> (Int, Int)
goldbach n = case find goldbachPred searchSpace of
  Just g  -> (g, (n-g))
  Nothing -> (-1, -1)
  where goldbachPred x  = isPrime $ n - x
        searchSpace     = takeWhile (< n) primes
