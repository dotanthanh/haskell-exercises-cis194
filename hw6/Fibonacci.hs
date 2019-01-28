{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- ex 1 -- 

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- ex 2 --

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- ex 3 --

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show s = show . take 40 $ streamToList s


streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

-- ex 4 --

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))

-- ex 5 --

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (streamRepeat <$> [0..])
                 

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a as) s = Stream a (interleaveStreams s as)

-- ex 6 --
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)
    (Stream a1 s1) + (Stream a2 s2) = Stream (a1+a2) (s1+s2)
    negate = streamMap negate 
    (Stream a1 s1) * s@(Stream a2 s2) = Stream (a1*a2) (streamMap (*a1) s2 + s1 * s)

instance Fractional (Stream Integer) where
    (Stream a1 s1) / (Stream a2 s2) = q
        where q = Stream (a1 `div` a2) (streamMap (`div` a2) (s1 - q * s2))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)