{-# OPTIONS_GHC -Wall -osuf hs #-}

-- ex 1 --

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f 
    where f n | even n = n `div` 2 | otherwise = 3 * n + 1

-- ex 2 --

type Height = Integer

data Tree a = Leaf | Node Height (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertToTree Leaf

getHeight :: Tree a -> Height
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

insertToTree :: a -> Tree a -> Tree a
insertToTree n Leaf = Node 0 Leaf n Leaf
insertToTree n (Node h l a r)
    | hl > hr = Node h l a (insertToTree n r)
    | hl < hr = Node h (insertToTree n l) a r
    | otherwise = Node (h' + 1) l' a r
    where 
        hl = getHeight l
        hr = getHeight r
        h' = getHeight l'
        l' = insertToTree n l

-- ex 3 --

xor :: [Bool] -> Bool
xor = foldl1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldl f' []
    where f' x' a = x' ++ [f a]

    -- 2nd implementation of map':  map' f = foldr ((:) . f) []      

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' = foldr . flip

-- ex 4 --

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\a -> 2 * a + 1) $ filter f list
    where 
        f a = notElem a [ x | (i, j) <- cartProd list list, let x = i + j + 2 * i * j, x <= n]
        list = [1..n]