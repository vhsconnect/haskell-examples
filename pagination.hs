{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
-- collection   = ['a','b','c','d','e','f']
-- itemsPerPage = 4
--
-- pageCount collection itemsPerPage       `shouldBe` 2
-- itemCount collection itemsPerPage       `shouldBe` 6
--
-- pageItemCount collection itemsPerPage 0 `shouldBe` Just 4 -- four of six items
-- pageItemCount collection itemsPerPage 1 `shouldBe` Just 2 -- the last two items
-- pageItemCount collection itemsPerPage 3 `shouldBe` Nothing -- page doesn't exist
--
-- pageIndex collection itemsPerPage  0    `shouldBe` Just 0 -- zero based index
-- pageIndex collection itemsPerPage  5    `shouldBe` Just 1 
-- pageIndex collection itemsPerPage 20    `shouldBe` Nothing
-- pageIndex collection itemsPerPage (-20) `shouldBe` Nothing
module Code where

type Collection a = [a]

type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n
  | mod (itemCount xs) n == 0 = (itemCount xs) `div` n
  | otherwise = (1 +) $ (itemCount xs) `div` n

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page
  | length xs == 0 = Nothing
  | page < 0 || n < 0 = Nothing
  | page > (pageCount xs n) - 1 = Nothing
  | page < (pageCount xs n) - 1 = Just n
  | mod (itemCount xs) n == 0 = Just n
  | otherwise = Just $ mod (itemCount xs) n

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item
  | length xs == 0 = Nothing
  | item < 0 || n <= 0 = Nothing
  | item > (itemCount xs) - 1 = Nothing
  | item == 0 = Just 0
  | item `mod` n == 0 = Just $ (1 -) $ item `div` n
  | otherwise = Just $ item `div` n
