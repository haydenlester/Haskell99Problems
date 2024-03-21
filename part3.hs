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
