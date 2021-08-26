+---+                                       +---+
|   |                                       |   |
+---+                                       +---+
+---++---+     +---+              +---++---++---+
|   ||   |     |   |   -->        |   ||   ||   |
+---++---+     +---+              +---++---++---+
+---++---++---++---+         +---++---++---++---+
|   ||   ||   ||   |         |   ||   ||   ||   |
+---++---++---++---+         +---++---++---++---+



Examples

gravityFlip 'R' [3, 2, 1, 2]     ->  [1, 2, 2, 3]
gravityFlip 'L' [1, 4, 5, 3, 5]  ->  [5, 5, 4, 3, 1]


----
----

module GravityFlip (gravityFlip) where

import Data.List (sort)

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip d   
  | d == 'R' = sort 
  | otherwise = reverse . sort 


-----

module GravityFlip (gravityFlip) where
import Data.List

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip c xs 
          | c == 'L' = reverse sl
          | c == 'R' = sl
          where sl = sort xs

-----

{-# LANGUAGE LambdaCase #-}
module GravityFlip (gravityFlip) where

import Data.List (sortBy)

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip = sortBy . \case 'R' -> compare; 'L' -> flip compare
