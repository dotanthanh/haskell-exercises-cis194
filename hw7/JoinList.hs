{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

-- ex 1 --

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- ex 2 --

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a)
    | n > 0 = Nothing
    | otherwise = Just a
indexJ n (Append b a1 a2)
    | rootSize <= n || n < 0 = Nothing
    | n < leftSize = indexJ n a1
    | otherwise = indexJ (n - leftSize) a2
    where leftSize = getSize . size $ tag a1
          rootSize = getSize . size $ b

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append b a1 a2)
    | rootSize <=n || n < 0 = Empty
    | n < leftSize = dropJ n a1 +++ a2
    | otherwise = dropJ (n - leftSize) a2
    where leftSize = getSize . size $ tag a1
          rootSize = getSize . size $ b

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ _ l@(Single _ _) = l
takeJ n l@(Append b a1 a2)
    | n >= rootSize = l
    | n <= 0 = Empty
    | n < leftSize = takeJ n a1
    | otherwise = a1 +++ takeJ (n - leftSize) a2
    where leftSize = getSize . size $ tag a1
          rootSize = getSize . size $ b

-- ex 3 --

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- ex 4 --

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ a1 a2) = jlToList a1 ++ jlToList a2

instance Buffer (JoinList (Score, Size) String) where 
    toString = unlines . jlToList

    fromString = foldr1 (+++) . map (\str -> Single (scoreString str, Size 1) str) . lines
    
    line = indexJ
    
    replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n+1) jl

    numLines = getSize . snd . tag

    value jl = n
        where Score n = fst . tag $ jl

initialLines :: [String]
initialLines =
    [   "This buffer is for notes you don't want to save, and for"
        , "evaluation of steam valve coefficients."
        , "To load a different file, type the character L followed"
        , "by the name of the file."
    ]

initialJoinList :: JoinList (Score, Size) String
initialJoinList = fromString (unlines initialLines)

main :: IO()
main = runEditor editor initialJoinList
