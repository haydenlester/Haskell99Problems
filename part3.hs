import System.Random

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x arr i = take (i-1) arr ++ (x : drop (i-1) arr)

-- 22
range :: Int -> Int -> [Int]
range l r = [l..r]

-- remove elt
removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

-- random index
rndI :: RandomGen g => [a] -> g -> (Int, g)
rndI = randomR . (\x -> (0, (length x)-1))

-- 23 full recursive
rndSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect [] _ gen = ([], gen)
rndSelect _ 0 gen  = ([], gen)
rndSelect l c gen
  | c == (length l)   = (l, gen)
  | otherwise         = rndSelect (removeAt k l) c gen'
                        where (k, gen') = rndI l gen

-- 23
rndSelectIO :: [a] -> Int -> IO [a]
rndSelectIO l c = getStdRandom $ rndSelect l c
