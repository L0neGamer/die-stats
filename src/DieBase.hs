module DieBase
    where

import Data.Map (empty, Map, insertWith, fromListWith, toList)
import qualified Data.Map as M
import Data.List as L (sort, sortOn)

data OperatorMod = Low | High deriving Show

-- data RerollType = RerollOnce (Int -> Bool) | RerollInfinite (Int -> Bool)
-- data RerollType = RerollOnce (Int -> Bool)

-- data Operator = Keep OperatorMod Int | Drop OperatorMod Int | Reroll RerollType | ExplodeOn Int | Min Int | Max Int deriving Show
-- data Operator = Keep OperatorMod Int | Drop OperatorMod Int | Reroll RerollType | Min Int | Max Int deriving Show
data Operator = Keep OperatorMod Int | Drop OperatorMod Int | Min Int | Max Int deriving Show

data Die = DieBase Int | DieMult Int Die | DieOp Die Operator | DieMod Die Int | DieAdd Die Die deriving Show
-- dX | Y[dice] | [dice][op] | [dice] + Z

-- type ExpandedDie = [Die]

-- instance Show RerollType where
--     show (RerollOnce _) = "RerollOnce"
    -- show (RerollInfinite _) = "RerollInfinite"

type DiceCollection = [([Int], Int)]

condenseDice :: DiceCollection -> DiceCollection
condenseDice ds = toList $ fromListWith (+) ds

-- map of values to frequencies
getProbs' :: DiceCollection -> Map Int Int
getProbs' d = fromListWith (+) [(val, count) | (val, count) <- summed]
    where summed = map (\(vals, count) -> (sum vals, count)) d

getProbs :: Die -> Map Int Int
getProbs d = getProbs' (expandDie d)

getPercentages :: Die -> Map Int Float
getPercentages d = M.map (\x -> 100 * (((/total) . fromIntegral) x)) probs
    where probs = getProbs d
          total = (fromIntegral $ foldr (+) 0 probs)

expandMult' :: DiceCollection -> DiceCollection -> DiceCollection
expandMult' [] _ = []
expandMult' ((x, y):xs) ys = map (\(x', y') -> (x++x', y)) ys ++ (expandMult' xs ys)

expandMult :: Int -> DiceCollection -> DiceCollection
-- expandMult 0 xs = xs
expandMult x xs
    | x == 0 = []
    | x == 1 = xs
    | x > 1 = expandMult' xs (expandMult (x-1) xs)
    | otherwise = error "negative expansion"

getOp :: Operator -> ([Int] -> [Int])
getOp (Max i) xs = map (min i) xs
getOp (Min i) xs = map (max i) xs
getOp (Keep High i) xs = take i $ sortOn (\x -> -x) xs
getOp (Keep Low i) xs = take i $ sort xs
getOp (Drop High i) xs = drop i $ sortOn (\x -> -x) xs
getOp (Drop Low i) xs = drop i $ sort xs

expandDie :: Die -> DiceCollection
expandDie (DieBase i)
    | i > 0 = map (\x -> ([x], 1)) [1..i]
    | otherwise = error "die value cannot be less than 1"
expandDie (DieMult i d) = expandMult i (expandDie d)
expandDie (DieMod d i) = map (\(x,y) -> (map (+i) x, y)) (expandDie d)
expandDie (DieOp d op) = condenseDice $ map (\(x,y) -> (dieOp x, y)) (expandDie d)
    where dieOp = getOp op

d20 = DieBase 20
roll2 d = DieMult 2 d

adv = DieOp (DieMult 2 d20) (Keep High 1)

highestOfLowest = DieOp (DieMult 2 (DieOp (DieMult 2 d20) (Keep Low 1))) (Keep High 1)
lowestOfHighest = DieOp (DieMult 2 (DieOp (DieMult 2 d20) (Keep High 1))) (Keep Low 1)

-- data ExpandedDie = Base [Int] | Mult ExpandedDie ExpandedDie deriving Show

-- expandMult :: Int -> ExpandedDie -> ExpandedDie
-- expandMult i d
--     | i > 1 = Mult d (expandMult (i-1) d)
--     | i == 1 = d
--     | otherwise = error "expand value is less than 1"

-- expandDie :: Die -> ExpandedDie
-- expandDie (DieBase i)
--     | i > 0 = Base [1..i]
--     | otherwise = error "die value is less than 1"
-- expandDie (DieMult i d) = expandMult i expanded
--     where expanded = expandDie d

-- expandDice :: Die -> ExpandedDie
-- expandDice (DieBase x) = [DieBase x]
-- expandDice (DieMult x d) = take (x * (length expanded)) (cycle expanded)
--     where expanded = expandDice d
-- expandDice (DieMod d i) = map (flip (DieMod ))

