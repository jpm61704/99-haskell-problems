import Problems1to10
import Data.List.Split

--problem 11
data Compress a = Single a | Multiple Int a deriving (Show)

encode' :: Eq a => [a] -> [Compress a]
encode' x = map (\x -> if length x /= 1 then Multiple (length x) (head x) else Single (head x)) $ pack x

--problem 12
decode :: Eq a => [Compress a] -> [a]
decode = foldr decodeCompress []
    where decodeCompress (Single x) y = x : y
          decodeCompress (Multiple n x) y = replicate n x ++ y

--problem 13
--skipped problem 13, example of using accumilator passing style

--problem 14
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

--problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

--problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = concatMap (\x -> if length x == n then init x else x ) (chunksOf n xs)

--problem 17
mySplit :: [a] -> Int -> ([a],[a])
mySplit x n
    | n < 0 || n > length x = error "Out of Bounds Error"
    | otherwise = mySplit' x [] ((length x) - n)
    where mySplit' x y 0 = (x,y)
          mySplit' x y n = mySplit' (init x) (last x : y) (n-1)

--problem 18
slice :: [a] -> Int -> Int -> [a]
slice x n m = fst $ mySplit (snd (mySplit x (n-1))) (m-n+1)

--problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = reverse $ reverse ys ++ reverse zs
    where (ys,zs) = mySplit xs m
          m = if n > 0 then n `mod` len else (len + n) `mod` len
          len = length xs

-- problem 20
