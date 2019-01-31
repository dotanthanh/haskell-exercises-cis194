{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances #-}

module Scrabble where

import Data.Monoid
import Data.List
import Data.Char (toUpper)

-- ex 3 --

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

scores :: [(Char, Int)]
scores = 
    [
        ('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1), ('F', 4), ('G', 2), ('H', 4), ('I', 1),
        ('J', 8), ('K', 5), ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3), ('Q', 10), ('R', 1),
        ('S', 1), ('T', 1), ('U', 1), ('V', 4), ('W', 4), ('X', 8), ('Y', 4), ('Z', 10)
    ]

score :: Char -> Score
score c = Score $ maybe 0 snd a 
    where a = find (\(x, _) -> x == toUpper c) scores

-- be aware of pitfall with foldr1/foldl1 where the list provided CANNOT be empty
-- mconcat can also an option here 
scoreString :: String -> Score
scoreString s = foldr (<>) mempty $ map score s
