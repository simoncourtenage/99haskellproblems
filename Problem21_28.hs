module Problem21_28 where

import System.Random 
import Data.List (sortBy,groupBy,(\\))

-- Problem 21 - insert a value at a given position in a list

-- might as well do some bounds checking
insertAt :: a -> [a] -> Int -> [a]
insertAt a as n | n < 1         = as
                | n > length as = as 
                | otherwise     = take (n-1) as ++ [a] ++ drop (n-1) as

-- Problem 22 - create list containing all integers within a given range

range :: Integer -> Integer -> [Integer]
range min max = [min .. max]

-- Problem 23 - extract a given number of randomly selected values from a list

-- this doesn't choose distinct elements, since randoms can recur
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do 
    g <- newStdGen
    let ns = randomRs (0,length xs - 1) g
    -- these two lines were my original version.  When testing, though, every so often
    -- it only produced two characters - eventually realised it was because sometimes
    -- duplicate numbers would appear in the three selected random numbers
    -- let ixs = take n $ randomRs (1,length xs) g
    -- return $ (map snd . filter (flip elem ixs . fst)) (zip [1..] xs)
    return $ take n . map (xs!!) $ ns

-- Problem 24 - draw N different random numbers from set 1..M

-- This avoids use of nub, but does incur a call to elem for each number generated!
-- If the total numbers requested is greater than the numbers in the given range, then
-- problem can't be solved, so just return empty list.
diff_select :: Int -> Int -> IO [Int]
diff_select n m
    | n > m = return [] -- if the number of distinct randoms is greater than length of range
    | otherwise 
        = do
            g <- newStdGen
            let ns = randomRs (1,m) g
            return $ take n (getNewInts g m [])

getNewInts :: RandomGen g => g -> Int -> [Int] -> [Int]
getNewInts gen max nums | elem n nums = getNewInts g' max nums
                        | otherwise   = n : getNewInts g' max (n:nums)
    where (n,g') = randomR (1,max) gen

-- Problem 25 - generate random permutation of elements in a list

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
    let l = length xs
    ns <- diff_select l l
    return $ map snd . sortBy (\x y -> compare (fst x) (fst y)) . zip ns $ xs

-- Problem 26 - generate combinations of K distinct objects from a list of length B

combinations :: Int -> [a] -> [[a]]
combinations 0 xs = []
combinations _ [] = []
combinations n xss@(x:xs)
    | length xss < n  = []
    | n == 1          = map (:[]) xss
    | length xss == n = [xss]
    | otherwise       = (map (x:) $ combinations (n-1) xs) ++ combinations n xs

-- Problem 27 - group elements of a set into disjoint subsets

-- (a) assume subsets of 2, 3 and 4 on a list of length 9
group3 :: Eq a => [a] -> [[[a]]]
group3 xs
    | length xs /= 9 = [] -- sanity check
    | otherwise      = concatMap (f xs) (combinations 2 xs)
    where f ys s = map (s:) (concatMap (g (ys \\ s)) (combinations 3 (ys \\ s)))
          g ys s = map (s:) [(combinations 4 (ys \\ s))]

-- (b) generalise for list of subset lengths

group' :: Eq a => [Int] -> [a] -> [[[a]]]
group' [] _ = []
group' [l] xs = combinations l xs : []
group' (l:ls) xs = concatMap (f xs) (combinations l xs)
    where f ys s = map (s:) $ group' ls (ys \\ s)

-- Problem 28 (a) - sort list of lists according to length of sublists

-- can't quite think of a snappier way to write the sortBy function argument
lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

-- Problem 28 (b) - sort list by frequency of length

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy (\x y -> length x == length y) . lsort