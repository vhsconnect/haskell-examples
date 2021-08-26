-- Observe the process with the array given below and the tracking of the sums of each corresponding array.
--
-- [5, 3, 6, 10, 5, 2, 2, 1] (34) ----> [5, 3, 6, 10, 2, 1] ----> (27) ------> [10, 6, 5, 3, 2, 1]  ----> [4, 1, 2, 1, 1] (9) -----> [4, 1, 2] (7)
--
-- The tracked sums are : [34, 27, 9, 7]. We do not register one of the sums. It is not difficult to see why.
--
-- We need the function track_sum ( or trackSum ) that receives an array ( or list ) and outputs a tuple ( or array ) with the following results in the order given below:
--
--     array with the tracked sums obtained in the process
--     final array
--
-- So for our example given above, the result will be:
--
--
--
-- trackSum [ 5, 3, 6, 10, 5, 2, 2, 1 ] == ( [ 34, 27, 9, 7 ], [ 4, 1, 2 ] )


module TrackSum (trackSum) where
import Data.List hiding (insert, nub)
import Data.Set (empty, insert, member)

fs = [nub, subNext, nub . sortBy (flip compare), id]

-- replacing nub to pass tests with large data sets, ugh ...
nub :: (Ord a) => [a] -> [a]
nub = nub' empty
  where
    nub' _ [] = []
    nub' set (x:xs)
      | member x set = nub' set xs
      | otherwise = x : nub' (insert x set) xs

subNext :: [Int] -> [Int]
subNext (x:xs)
  | (x : xs) == [] = []
  | xs == [] = []
  | otherwise = (x - head xs) : subNext xs

trackSum :: [Int] -> ([Int], [Int])
trackSum arr = trackSum' ([], arr) (length fs - 1)
  where
    trackSum' :: ([Int], [Int]) -> Int -> ([Int], [Int])
    trackSum' tup step
      | step >= 0 =
        let evaluated = (fs !! step) (snd tup)
         in trackSum' ((fst tup) ++ [sum evaluated], evaluated) (step - 1)
      | otherwise = tup


--------------

module TrackSum (trackSum) where

import Data.List (nub, sortBy, group)

trackSum :: [Int] -> ([Int],[Int])
trackSum xs = (map sum [xs, xs', xs'', xs'''], xs''')
  where xs'    = (map head . group . sortBy (flip compare)) xs
        xs''   = zipWith (-) xs' (tail xs')
        xs'''  = nub xs''


----------------------

module TrackSum (trackSum) where

import Data.Function ((&))
import Data.List (sort)
import qualified Data.Set as Set

trackSum :: [Int] -> ([Int], [Int])
trackSum xs = (sum <$> lists, last lists)
  where
    lists = scanl (&) xs [ordNub, diffs . reverse . sort, ordNub]

ordNub :: Ord a => [a] -> [a]
ordNub = go mempty
  where
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs
    go _ [] = []

diffs :: [Int] -> [Int]
diffs (a : b : xs) = a - b : diffs (b : xs)
diffs [_] = []
diffs [] = []

-----------------------------

module TrackSum (trackSum) where

import Control.Monad.Writer (Writer,runWriter,tell,(>=>))
import Data.Tuple (swap)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.IntSet (empty,member,insert)

trackSum :: [Int] -> ([Int],[Int])
trackSum = swap . runWriter . process

process :: [Int] -> Writer [Int] [Int]
process =   track
       >=> return . nubInt
       >=>  track
       >=> return . sortOn Down
       >=> return . (zipWith (-) <*> tail)
       >=>  track
       >=> return . nubInt
       >=>  track

track :: [Int] -> Writer [Int] [Int]
track xs = do
  tell [sum xs]
  return xs

nubInt :: [Int] -> [Int]
nubInt = go empty where
  go _ [] = []
  go s (x:xs) | x `member` s = go s xs
              | otherwise = x : go (insert x s) xs

----------

module TrackSum (trackSum) where

import Data.List (sortBy)
import Data.IntSet (member,insert)

trackSum :: [Int] -> ([Int],[Int])
trackSum l1 = ( sum <$> [l1,l2,l3,l4] , l4 ) where
  l2 = nubInt l1
  lx = sortBy (flip compare) l2
  l3 = zipWith (-) <*> tail $ lx
  l4 = nubInt l3

nubInt :: [Int] -> [Int]
nubInt = go mempty where
  go _ [] = []
  go s (x:xs) | x `member` s = go s xs
              | otherwise = x : go (insert x s) xs

-----
module TrackSum where
import Data.List

trackSum :: [Int] -> ([Int],[Int])
trackSum xs = let xs' = nub xs
                  xs'' = reverse $ sort xs'
                  xs''' = zipWith (-) (init xs'') (tail xs'')
                  xs'''' = nub xs'''
              in (map sum [xs, xs', xs''', xs''''], xs'''')

-----
--
module TrackSum where
import Data.Char
import Data.List
trackSum :: [Int] -> ([Int],[Int])
trackSum xs  = 
       let  a = sum xs
            b = sum $ nub xs 
            ld = reverse $  sort $ nub xs 
            chelp = zipWith (-) ld (tail ld)
            c = sum $ chelp
            d = nub $ chelp
            e = sum d
       in ([a,b,c,e],d)     




