-- The maximum sum subarray problem consists in finding the maximum sum of a contiguous subsequence in an array or list of integers:
--
-- maxSequence [-2, 1, -3, 4, -1, 2, 1, -5, 4]
-- -- should be 6: [4, -1, 2, 1]
--
-- Easy case is when the list is made up of only positive numbers and the maximum sum is the sum of the whole array. If the list is made up of only negative numbers, return 0 instead.
--
-- Empty list is considered to have zero greatest sum. Note that the empty list or array is also a valid sublist/subarray.
--

module MaxSequence where

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence = maximum . scanl (\acc x -> max 0 acc + x) 0


-------------------------


module MaxSequence where
import Control.Monad
import Data.List

maxSequence :: [Int] -> Int
maxSequence = maximum . (map sum) . subsets

subsets = tails >=> inits

----------------------------------

module MaxSequence where
import Data.List

maxSequence :: [Int] -> Int
maxSequence = maximum . map sum . concatMap inits . tails

-------------------------------------

module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence = maxSequence' 0 0 . scanl (+) 0 where
  maxSequence' _    best []     = best
  maxSequence' base best (x:xs) = maxSequence' (min base x) (max best (x - base)) xs

-----------------------------
module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence = maximum . scanl ((+) . max 0) 0

-----------------------------

module MaxSequence where

import Data.List
maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence xs = max 0 . maximum . concatMap (scanl1 (+)) . tails $ xs

-----------------------------

module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence = fst . foldl f (0, 0)
    where
        f (y, t) x = if t' > y then (t', t') else (y, t')
            where
                t' = if t > 0 then t + x else x

-----------------------

module MaxSequence where

import Data.List

maxSequence :: [Int] -> Int
maxSequence = getMaximum . map (getMaximum . map sum . tails). inits
    where
        getMaximum [] = 0
        getMaximum x = maximum x


------------------------
module MaxSequence where

import Data.List

maxSequence :: [Int] -> Int
maxSequence []     = 0
maxSequence (x:xs) = max a b
  where
    a = x + maximum (fmap sum $ inits xs)
    b = maxSequence xs

-------------------
module MaxSequence where


maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence (x:xs) = max (processAtIndex0 (x : xs)) (maxSequence xs)
  where
    processAtIndex0 [] = 0
    processAtIndex0 arr =
      max (foldl (+) 0 arr) (processAtIndex0 (take (length arr - 1) arr))

