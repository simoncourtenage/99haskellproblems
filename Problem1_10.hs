module Problem1_10 where

import Data.List
import Control.Arrow ((&&&))

{--
  Solution to the first 10 of the 99 Haskell Problems
  Found at https://wiki.haskell.org/99_questions/1_to_10
--}

-- Problem 1 - find last element of list

myLast :: [a] -> a
myLast = foldr1 (flip const)

-- Problem 2 - find last but one element of list

myButLast :: [a] -> a
myButLast = fst . myLast . (zipWith (,) <*> tail)

-- Problem 3 - find K'th element of list
-- This is a little overboard...
elementAt :: [a] -> Integer -> a
elementAt xs n = snd . head . filter ((==n) . fst) $ (zipWith (,) [1..] xs)

-- Problem 4 - find length of list

myLength :: [a] -> Int
myLength = foldl (\s x -> s+1) 0

-- Problem 5 - reverse a list

myReverse :: [a] -> [a]
myReverse = foldl' f []
    where f xs x = x : xs

-- Problem 6 - Is a list a palindrome

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = ((==) <*> reverse)

-- Problem 7 - flatten a nested list

data NestedList a = Elem a | List [NestedList a]
    deriving (Eq,Show)

instance Foldable NestedList where
    foldr f z (Elem a) = f a z
    foldr f z (List []) = z
    foldr f z (List ((Elem a):ls)) = f a $ foldr f z (List ls)
    foldr f z (List ((List l):ls)) = foldr f z' (List l)
        where z' = foldr f z (List ls)

flatten :: NestedList a -> [a]
flatten = foldr (:) []

testlist = (List [Elem 1, List [],List [Elem 2, List [Elem 3, Elem 4], List [],Elem 5]])

-- Problem 8 - eliminate consecutive duplicates in a list

compress :: Eq a => [a] -> [a]
compress = map head . group

-- Problem 9 - put consective duplicates into sublists
-- A home-grown version of group
-- Of all these 10 problems, this took the longest to solve. I knew roughly what I wanted to do, but working it out
-- and satisfying the typechecker took a little experimentation.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = (:) <$>  takeWhile  (==hd) <*> pack . dropWhile (==hd) $ xs
    where hd = head xs

-- Problem 10 - run-length encoding of a list
-- E.g., "aabbbcddd" => [(2,'a'),(3,'b'),(1,'c'),(3,'d')]

encode :: Eq a => [a] -> [(Int,a)]
encode = map (length &&& head) . pack