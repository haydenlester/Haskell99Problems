-- data type for 11+
data Encoded a = Single a | Multiple Int a
  deriving (Show)

-- 11
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper(1, x) = Single x
    encodeHelper(n, x) = Multiple n x

-- 12
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap mapper
  where
    mapper (Single x)     = [x]
    mapper (Multiple n x) = replicate n x

-- 13
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' x 1 xs

encodeDirect' :: Eq a => a -> Int -> [a] -> [Encoded a]
encodeDirect' x n xs
  | xs == [] = [encodeElement x n]
  | x == (head xs)  = encodeDirect' x (n+1) (tail xs)
  | otherwise       = encodeElement x n : encodeDirect xs

encodeElement :: a -> Int -> Encoded a
encodeElement x n
  | n == 1    = Single x
  | otherwise = Multiple n x

-- 14
duplicate :: [a] -> [a]
duplicate = concatMap (\x -> [x, x])

-- 15
replicate' :: [a] -> Int -> [a]
replicate' xs n = concatMap (replicate n) xs

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (x, c) <- (zip xs [1,2..]), (mod c n) /= 0]

-- 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs l r = take (r-l+1) $ drop (l-1) xs

-- 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
  | n > 0     = drop n xs ++ take n xs
  | n < 0     = rotate xs (length xs + n)
  | otherwise = xs

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = (head $ drop (i-1) xs, take (i-1) xs ++ drop i xs)
