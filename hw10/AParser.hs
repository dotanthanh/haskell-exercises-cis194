module AParser where

import           Control.Applicative

import           Data.Char

import           Data.Maybe (isNothing)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
    where
    f xs
        | null ns   = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- ex 1 --

instance Functor Parser where
    fmap f (Parser x) = Parser (fmap (first f) . x)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- ex 2 --

-- a <$> b is similar to fmap a b
instance Applicative Parser where
    pure a = Parser f
        where f str = Just (a, str)

    x <*> y = Parser z
        where   z str
                    | isNothing $ runParser x str = Nothing
                    | otherwise = first a <$> runParser y sx 
                    where   Just (a, sx) = runParser x str

-- ex 3 --

abParser :: Parser (Char, Char)
abParser = pure (,) <*> char 'a' <*> char 'b' 

abParser_ :: Parser ()
abParser_ = pure (const ()) <*> abParser

intPair :: Parser [Integer]
intPair = pure (\a _ b -> [a, b]) <*> posInt <*> char ' ' <*> posInt

-- ex 4 --

instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    
    x <|> y = Parser f
        where f str = runParser x str <|> runParser y str

-- ex 5 --

intOrUppercase :: Parser ()
intOrUppercase = pure (const ()) <*> posInt <|> pure (const ()) <*> satisfy isUpper
