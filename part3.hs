import System.Random

-- 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take (i-1) xs ++ (x : drop (i-1) xs)

-- 22
-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range l r = [l..r]

-- remove elt
removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

-- random index
rndI :: RandomGen g => [a] -> g -> (Int, g)
rndI xs = randomR (0, (length xs)-1)

-- 23
-- Extract a given number of randomly selected elements from a list.
rndSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect [] _ gen = ([], gen)
rndSelect _ 0 gen  = ([], gen)
rndSelect xs c gen
  | c == (length xs)  = (xs, gen)
  | otherwise         = rndSelect (removeAt i xs) c gen'
                        where (i, gen') = rndI xs gen

rndSelectIO :: [a] -> Int -> IO [a]
rndSelectIO xs c = getStdRandom $ rndSelect xs c

-- 24
-- Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelectIO [1..m] n
