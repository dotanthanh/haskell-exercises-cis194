module Golf where

import Data.List

skips :: [a] -> [[a]]
skips x = [ skipWithStep i $ drop (i-1) x | i <- [1..n]] where n = length x

skipWithStep :: Int -> [a] -> [a]
skipWithStep _ [] = []
skipWithStep n (x:xs) = x : (skipWithStep n $ drop n (x:xs))


localMaxima :: [Integer] -> [Integer]
localMaxima x@(x1:x2:x3:xs) 
    | maximum [x1,x2,x3] == x2 = [x2] ++ (localMaxima $ drop 2 x)
    | otherwise = localMaxima $ drop 1 x
localMaxima _ = []    

histogram :: [Integer] -> String
histogram x = (unlines $ reverse . transpose $ getBars $ getListOfEach x) ++ "==========\n0123456789\n"

getListOfEach :: [Integer] -> [Int]
getListOfEach x = [ length $ filter (== i) x | i <- [0..9] ]

getBars :: [Int] -> [String]
getBars x = [ replicate m '*' ++ replicate (maximum x - m) ' ' | m <- x ]  