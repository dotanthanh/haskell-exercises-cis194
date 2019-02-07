{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Ex 2 --

dieMany :: Int -> Rand StdGen [DieValue]
dieMany n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield ba bd)  = x >>= f
    where    x  = dieMany (att + def)
             f d = return (Battlefield (ba - fst a) (bd - snd a))
                 where a = battleResult (att, def) d
             att = min (ba - 1) 3
             def = min bd 2

battleResult :: (Army, Army) -> [DieValue] -> (Army, Army)
battleResult a v = computeCasualties vs (0, 0) 
    where vs = (\(a, b) -> (sort a, sort b))  $ splitAt (fst a) v

computeCasualties :: ([DieValue], [DieValue]) -> (Army, Army) -> (Army, Army)
computeCasualties ([], _) a = a
computeCasualties (_, []) a = a
computeCasualties ((a:as), (d:ds)) (aa, ad)
    | a > d = computeCasualties (as, ds) (aa, ad +1)
    | otherwise = computeCasualties (as, ds) (aa + 1, ad)

-- Ex 3 --

invade :: Battlefield -> Rand StdGen Battlefield
invade b
    | attackers b < 2 = return b
    | defenders b <= 0 = return b
    | otherwise = battle b >>= invade

-- Ex 4 --

successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>= computeProb

computeProb :: [Battlefield] -> Rand StdGen Double
computeProb bfs  = return $ x / y
    where x = fromIntegral (length $ filter ((== 0) . defenders) bfs) :: Double
          y = fromIntegral (length bfs) :: Double

exampleProb = do
    values <- evalRandIO $ successProb (Battlefield 30 15)
    putStrLn "30 attackers, 15 defenders"
    putStrLn (show (values * 100) ++ "% chance of winning")
