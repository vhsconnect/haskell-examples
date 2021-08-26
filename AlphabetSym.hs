
-- Consider the word "abode". We can see that the letter a is in position 1 and b is in position 2. In the alphabet, a and b are also in positions 1 and 2. Notice also that d and e in abode occupy the positions they would occupy in the alphabet, which are positions 4 and 5.
--
-- Given an array of words, return an array of the number of letters that occupy their positions in the alphabet for each word. For example,


-- solve(["abode","ABc","xyzD"]) = [4, 3, 1]
--

module AlphabetSymm where
import Distribution.Simple.Utils


solve :: [String] -> [Int]
solve [] = []
solve (x:xs) =
  (
  sum 
  $ map fromEnum
  $ zipWith (==) (lowercase x) ['a' .. 'z']
  ) : solve xs


------
--
--

module AlphabetSymm where

import Data.Char

solve :: [String] -> [Int]
solve = map solve' where
  solve' = length . filter id . zipWith (==) ['a'..'z'] . map toLower


------------------
--

module AlphabetSymm where

import Data.Char (ord)
import Data.Char (toLower)

solve :: [String] -> [Int]
solve = map 
(length . (filter (==True)) . (zipWith (==) ['a'..]) . (map toLower))


-----

module AlphabetSymm where

import Data.Char

solve :: [String] -> [Int]
solve = map (sum . zipWith (\x y -> if x == toLower y then 1 else 0) ['a'..'z'] )


------


module AlphabetSymm (solve) where

import Data.Char (toLower)

solve :: [String] -> [Int]
solve = map $ sum . map fromEnum . zipWith (==) ['a'..] . map toLower

--------

module AlphabetSymm (solve) where

import Data.Char (toLower)

solve :: [String] -> [Int]
solve = map (solve' . map toLower) where
  solve' :: String -> Int
  solve' = sum . zipWith ((fromEnum .) . (==)) ['a'..]



