import Data.List (group)

-- 1
last' :: [a] -> a
last' = last

-- 2
butLast :: [a] -> a
butLast = last . init

-- 3
element :: [a] -> Int -> a
element = (!!)

-- 4
length' :: [a] -> Int
length' = length

-- 5
reverse' :: [a] -> [a]
reverse' = reverse

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- data type for 7+
data NestedList a = Elem a | List [NestedList a]

-- 7
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- 8
compress :: Eq a => [a] -> [a]
compress = map head . group

-- 9
pack :: Eq a => [a] -> [[a]]
pack = group

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group
