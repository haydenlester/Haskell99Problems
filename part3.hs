import System.Random
import List
import Data.Ord (comparing)

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

-- 23
-- Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect _ 0 = return []
rndSelect xs c = do 
    i <- randomRIO (0, (length xs)-1)
    rest <- rndSelect (removeAt i xs) (c-1)
    return $ (xs!!i) : rest

-- 24
-- Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- 25
-- Generate a random permutation of the elements of a list.
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs $ length xs

-- 26
-- Generate combinations of K distinct objects chosen from the N elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- from wiki
-- maps to tuple containing unused
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [(x:ys,zs) | (ys,zs) <- combination (n-1) xs]
    ds = [(ys,x:zs) | (ys,zs) <- combination n xs]

-- original approach used set difference rather than
-- constructing the unused set from scratch
-- composed into mapper. this is better.

-- 27
-- Calculate all disjoint subgroups of size [x1, x2..]
-- possible with the provided set
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (g:gs) xs = [y:ys |
                      (y, rs) <- combination n xs,
                      (ys)    <- group gs rs]

-- 28
-- Sort a list of lists, by length
lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)
