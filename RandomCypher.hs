-- Description:
-- Intro:
-- I was doing a coding challenge. It was one of those multi-step challenges. I don't know if my approach was good or bad, but in one of these steps I was writing a function to convert word to numbers. I did it.. eventually, but... I didn't like how it was written. So I thought why not create kata and check how other people do it :) So:
--
-- Task:
-- Your task is to write the word to number converter. Digits in the number should match letters in the word. Plus generated number should be the smallest possible number you can get.
--
-- Words will contain of maximum 10 distinct letters, but word can be any length, even longer than 10 characters long.
-- Number can NOT start with 0
-- Same letters share the same digit regardless of case
-- For empty string return 0
-- Examples:
-- "A" -> 1 - OK
--
-- "ABA" -> 353 - WRONG ( number is OK, but it's not the smallest number )
--
-- "ABA" -> 333 - WRONG ( different letters map to same digits )
--
-- "ABA" -> 357 - WRONG ( same letters map to different digits )
--
--



module WordToInitialNumber (convert) where

import Control.Applicative (liftA2)
import Data.Char (toLower)
import Data.List (find, nub)
import Data.Maybe (fromJust)
import Data.Semigroup (Sum (..))

convert :: String -> Int
convert = sumProds . (liftA2 zip (powers . length) mapChars) . (map toLower)
  where
    charMap = zip (1 : 0 : [2 .. 9]) . nub
    findN m c = fst $ fromJust $ find ((== c) . snd) m
    mapChars = liftA2 (map . findN) charMap id
    powers l = [10 ^ i | i <- [l -1, l -2 .. 0]]
    sumProds = getSum . foldMap (Sum . (uncurry (*)))
-----
-----
-----

module WordToInitialNumber (convert) where

import Data.List
import Distribution.Simple.Utils



digits = ["1", "0", "2", "3", "4", "5", "6", "7", "8", "9"]

zipWithIndex :: String -> [(Char, Int)]
zipWithIndex word =
  [(x, y) | x <- word, y <- map (maybe 0 id) $ map (elemIndex x) [nub word]]

cypher :: [(Char, Int)] -> String
cypher zipped = foldl (++) "" [digits !! x | x <- map snd zipped]


convert :: String -> Int
convert word 
  | word == "" = 0
  | otherwise = read . cypher . zipWithIndex $ lowercase word

----
--
--
--
module WordToInitialNumber (convert) where

import Data.List
import Data.Char

convert :: String -> Int
convert [] = 0
convert (_:[]) = 1
convert xs = readInt $ concatMap show $ map (index ys) zs
             where ys = nub' zs
                   zs = map toLower xs

nub' xs = if length ys >= 2
          then swap ys
          else ys
          where ys = nub xs

swap (a:b:xs) = b:a:xs
swap xs = xs

readInt x = read x :: Int

index ys c = if length ys == 1
             then 1
             else snd $ head $ dropWhile (\(z, i) -> z /= c) zs
             where zs = zip ys [0..]

-----
--
--
--


module WordToInitialNumber (convert) where

import Control.Monad.State
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map

type Mapping = Map Char Int

convert :: String -> Int
convert "" = 0
convert str = foldl ((+) . (* 10)) 0 . (`evalState` (Map.empty, 1:0:[2..])) . traverse (f . toLower) $ str

f :: Char -> State (Mapping, [Int]) Int
f c = do
  (mapping, is) <- get
  let (x:xs) = is
  maybe (put (Map.insert c x mapping, xs) >> pure x) pure $ Map.lookup c mapping

-----
--
--

module WordToInitialNumber (convert) where

import qualified Data.Map as Map
import           Data.Map   ( Map )
import Data.Char ( toLower )

type Mapping = Map Char Int

convert :: String -> Int
convert "" = 0
convert str = foldl ((+) . (*10)) 0 $ map ((fst mapping) Map.!) str'
  where
    str' = map toLower str
    mapping :: (Mapping, Int)
    mapping = foldl f (Map.empty, 1) str'

f :: (Mapping, Int) -> Char -> (Mapping, Int)
f (m, i) c = case Map.lookup c m of
               Nothing -> (Map.insert c i m, nextNum i)
               Just _ -> (m, i)

nextNum 1 = 0
nextNum 0 = 2
nextNum n = n + 1

----
--
--
--


module WordToInitialNumber (convert) where
import Data.Map (Map, member, insert, empty, (!))
import Data.Char (toLower)
import Control.Monad (liftM2, ap)
import Data.List (nub, reverse, map)

convert :: String -> Int
-- not so point-free
-- convert s_ = fst $ toNumber $ numbers (map toLower s_) $ fst $ lookups $ reverse $ nub $ map toLower s_
-- point-free equivalent:
convert = fst . toNumber . ap (numbers . map toLower) (fst . lookups . reverse . nub . map toLower)

lookups :: String -> (Map Char Int, Int)
lookups "" = (empty, 1)
lookups (c:cs) =
  (insert c num nextMap, inc num)
  where
    (nextMap, num) = lookups cs

inc :: Int -> Int
inc 0 = 2
inc 1 = 0
inc n = n + 1

numbers :: String -> Map Char Int -> [Int]
numbers = flip (map . (!))

toNumber :: [ Int ] -> (Int, Int)
toNumber [] = (0, 1)
toNumber (n:ns) = (n * place + nextN, place * 10)
  where
    (nextN, place) = toNumber ns 


-----
--
--
--
--
--

module WordToInitialNumber (convert) where
import Data.Char
import Data.List
import Data.Maybe

convert :: String -> Int
convert [] = 0
convert s = read.map intToDigit.helper $ map toLower s
    where
          helper xs
              |length xs' == 1 = replicate (length s) 1
              |otherwise = map (\x -> fromJust (elemIndex x _xs)) $ xs
              where xs' = nub xs
                    h1:h2:r = xs'
                    _xs = h2:h1:r

----
--
--
--
--

{-# Language ViewPatterns #-}

module WordToInitialNumber (convert) where

import Data.Char (toLower)
import Data.List (nub,elemIndex)

convert :: String -> Int
convert (map toLower -> s) = foldl ( \ z c -> 10 * z + t `indexOf` c ) 0 s where
  (x:y:zs) = nub s ++ "?"
  t = y:x:zs

indexOf :: (Eq a) => [a] -> a -> Int
xs `indexOf` ((`elemIndex` xs) -> Just r) = r



