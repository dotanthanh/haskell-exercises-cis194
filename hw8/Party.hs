{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Employee

import Data.Tree

-- ex 1 --

-- change to record syntax later for more concise, shorter code
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL el lf) = GL (e:el) (lf + f)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

instance Semigroup GuestList where
   (GL l1 f1) <> (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

-- ex 2 --

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root subs) = f root bl
    where bl = map (treeFold f) subs

-- ex 3 --

-- can use foldMap here
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e l = (glCons e withoutBoss, withBoss)
    where   withBoss = foldr (<>) mempty (map fst l)
            withoutBoss = foldr (<>) mempty (map snd l)

-- ex 4 --

-- fold the tree with nextLevel to find the optimal tuple (of GuestList with and without the boss) at the root of the whole tree
-- then evaluate the 2 values (fun-ness) of that tuple and get the moreFun one
maxFun :: Tree Employee -> GuestList
maxFun =  uncurry moreFun . treeFold nextLevel

joe :: Employee
joe = Emp "Joe" 5

sam :: Employee
sam = Emp "Sam" 3

sue :: Employee
sue = Emp "Sue" 3

hugo :: Employee
hugo = Emp "Hugo" 7

sergei :: Employee
sergei = Emp "Sergei" 1

maxFunTest :: Bool
maxFunTest = and
  [
    GL [joe] 5     == maxFun (Node joe []),
    GL [hugo, sergei, sue] 13 == maxFun (Node joe [Node sam [Node hugo [], Node sergei []], Node sue []])
  ]

-- ex 5 --

main :: IO ()
main = do
    contents <- readFile "company.txt"
    let (GL el f) = maxFun $ read contents
    putStrLn ("total fun: " ++ show f)
    mapM_ putStrLn (map empName el)