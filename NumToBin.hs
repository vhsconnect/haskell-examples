-- Task
-- You are given two numbers a and b where 0 ≤ a ≤ b. Imagine you construct an array of all the integers from a to b inclusive. You need to count the number of 1s in the binary representations of all the numbers in the array.
--
-- Example
-- For a = 2 and b = 7, the output should be 11
--
-- Given a = 2 and b = 7 the array is: [2, 3, 4, 5, 6, 7]. Converting the numbers to binary, we get [10, 11, 100, 101, 110, 111], which contains 1 + 2 + 1 + 2 + 2 + 3 = 11 1s.

-----
-----



module RangeBitCounting (rangeBitCount) where
import Data.Char (intToDigit)

rangeBitCount :: Int -> Int -> Int
rangeBitCount a b = length $ filter (/='0') $ foldl1 (++) $ map intToBin [a..b]
  where 
    intToBin x 
      | x == 0 = ['0']
      | x == 1 = ['1']
      | otherwise = let (q,r) = quotRem x 2 in intToBin q ++ [intToDigit r]



------------

module RangeBitCounting (rangeBitCount) where
import Data.Char
import Numeric

rangeBitCount :: Int -> Int -> Int
rangeBitCount a b = length (filter (/='0') (foldr ff "" [a..b]))
                    where
                      ff e acc = acc ++ showIntAtBase 2 intToDigit e ""


------------


module RangeBitCounting (rangeBitCount) where

import Data.Bits (popCount)

rangeBitCount :: Int -> Int -> Int
rangeBitCount a = sum . map popCount . enumFromTo a                     


------------

module RangeBitCounting (rangeBitCount) where
import Text.Printf

rangeBitCount :: Int -> Int -> Int
rangeBitCount a b = length $ filter (=='1') $ concat $ map (printf "%b") [a..b]


------------

module RangeBitCounting (rangeBitCount) where

countBinaryOnes :: Int -> Int
countBinaryOnes 0 = 0
countBinaryOnes 1 = 1
countBinaryOnes n = (n `mod` 2) + (countBinaryOnes $ n `div` 2)

rangeBitCount :: Int -> Int -> Int
rangeBitCount a b = sum $ map countBinaryOnes [a..b]

------------

module RangeBitCounting (rangeBitCount) where

toBin :: Int -> String
toBin x
  | x == 0 = "0"
  | x == 1 = "1"
  | mod x 2 == 0 = toBin (div x 2) ++ "0"
  | otherwise = toBin (div x 2) ++ "1"


countOnes :: String -> Int 
countOnes str 
  | str == "" = 0
  | otherwise = fromEnum ((head str) == '1') + countOnes (tail str)

rangeBitCount :: Int -> Int -> Int
rangeBitCount x y = sum $ map countOnes $ map toBin [x .. y]


----------------------


module RangeBitCounting (rangeBitCount) where

import Data.Char
import Numeric

rangeBitCount :: Int -> Int -> Int
rangeBitCount a b = sum (map (\n -> length (filter (=='1') n)) (map (\e -> showIntAtBase 2 intToDigit e "") [a..b]))
