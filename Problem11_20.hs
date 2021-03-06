module Problem11_20 where

import Data.List
import Control.Arrow ((&&&))
import Data.Tuple (swap)
import Problem1_10 (pack)

{--
  Solution to problems 11-20 of the 99 Haskell Problems
  Found at https://wiki.haskell.org/99_questions/11_to_20
--}

-- Problem 11 - modify solution to Problem 10 (run-length encoding)
-- so that if an item has no consecutive duplicates, it is just copied
-- to the output list

data Encoding a = Multiple Int a | Single a
    deriving (Show)

-- won't work for empty lists
listEncoding :: [a] -> Encoding a 
listEncoding xs | l == 1    = Single h
                | otherwise = Multiple l h
    where l = length xs
          h = head xs

-- could also build on top of encode from Problem 10, which would mean
-- listEncoding would work on all cases
encodingModified :: Eq a => [a] -> [Encoding a]
encodingModified 
    = map listEncoding . pack

-- Problem 12 - decode a list produced by encodeModified

-- A bit flash :) - could also use concatMap
decodeModified :: [Encoding a] -> [a]
decodeModified = (=<<) decodeHelper 

decodeHelper :: Encoding a -> [a]
decodeHelper (Single a) = [a]
decodeHelper (Multiple n a) = replicate n a

testencodinglist = [Multiple 2 'a',Single 'b',Single 'c',Multiple 4 'd',Multiple 2 'e']

-- Problem 13 - encode directly (without using pack, for example)

-- Just the pack code with listEncoding added.  Not sure why the 99 Problems
-- website thinks this is of intermediate difficulty.
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect xs = (:) <$> listEncoding . takeWhile (==hd)
                            <*> encodeDirect . dropWhile (==hd) $ xs
    where hd = head xs

-- Problem 14 - duplicate items in a list

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15 - replicate items in a list a given number of times

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16 - drop every N'th element from a list

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd . filter ((/=0) . (`mod` n) . fst) . zip [1..] $ xs

-- Problem 17 - split list into two parts without using predefined predicates

{--
   According to the solution page for this problem, this is "clearly using
   predefined predicates"! (though neither take nor drop has type a-> Bool)
--}
split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs,drop n xs)

-- so I offer this, written without "predicates"!
split' :: [a] -> Int -> ([a],[a])
split' [] _ = ([],[])
split' xs n = split_aux xs n []
    where split_aux [] _ h = (reverse h,[])
          split_aux xs 0 h = (reverse h,xs)
          split_aux (x:xs) n h = split_aux xs (n-1) (x:h)

-- Problem 18 - extract a slice from a list (count elements from 1)

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j-i+1) . drop (i-1) $ xs

-- Problem 19 - rotate a list N places to the left

rotate :: [a] -> Int -> [a]
rotate xs n | n == 0    = xs
            | n > 0     = drop n xs ++ take n xs
            | otherwise = drop n' xs ++ take n' xs
    where n' = length xs + n -- used when n is negative

-- Problem 20 - remove K'th element from list (count elements from 1)

-- Another of those little one-liners where you know roughly what you want to
-- do but it takes some experiment to get the uncurry/flip/(++) and (..) all in
-- the right place.  Also, should use Maybe in case the index is out of range.
removeAt :: Int -> [a] -> (a,[a])
removeAt n = (head . snd &&& uncurry (flip (++) . tail)) . swap . splitAt (n-1)

