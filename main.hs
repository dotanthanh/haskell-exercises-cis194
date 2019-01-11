import Data.List

-- this file contains only the initial practices for learning Haskell --

data Student = Person {
    lastname :: String,
    firstname :: String
} deriving (Show)

doubleMe x = (if x == 10 then x * 2 else 0) + 13

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | x <= maxTail = maxTail
    where maxTail = maximum' xs

replicate' :: (Num n, Ord n) => n -> x -> [x]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x      

take' :: (Num n, Ord n) => n -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs   

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs  
