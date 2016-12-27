module Problems1to10 where

-- problem 1
myLast :: [a] -> a
myLast = head . reverse

--problem 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

--problem 3
elementAt :: Int -> [a] -> a
elementAt n (x:xs)
    | n > length (x:xs) = error "out of bounds"
    | n == 1    = x
    | otherwise = elementAt (n-1) xs
elementAt _ _ = error "invalid arguments"

--problem 4
myLength :: [a] -> Integer
myLength = foldr (\_ y-> y+1) 0

--problem 5
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

--problem 6
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome x
    | length x == 1  = True
    | otherwise      = outers_match && palindrome inner
        where inner = init (tail x)
              outers_match = head x == last x

--problem 7
data NestedList a = Elem a | List [NestedList a] deriving(Show)

myFlatten :: NestedList a -> [a]
myFlatten (List x) = foldr (\x y-> myFlatten x ++ y) [] x
myFlatten (Elem x) = [x]

--problem 8

compress :: Eq a => [a] -> [a]
compress (x:y:xs)
    | x == y = compress (x:xs)
    | otherwise = x : compress (y:xs)
compress x = x

{-
--Alternatively
compress :: Eq a => [a] -> [a]
compress = map head . group
-}

--problem 9
pack :: Eq a => [a] -> [[a]]
pack x = if null rest then [group] else group : pack rest
    where split_list = span (== head x) x
          group      = fst split_list
          rest       = snd split_list

--problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode x = map (\x -> (length x, head x)) $ pack x

{- origional, above is reduced
encode x = zip group_counts group_names
    where groups = pack x
          group_counts = map length groups
          group_names = map (head) (compress groups)
-}
