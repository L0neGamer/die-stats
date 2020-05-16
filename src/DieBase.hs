module DieBase
    where

import Data.Map (Map, fromListWith, toList)
import qualified Data.Map as M
import Data.List as L (sort, sortOn)

data OperatorMod = Low | High deriving Show
data Operator = Keep OperatorMod Int | Drop OperatorMod Int | Min Int | Max Int deriving Show

data Die = Const Int | BaseDie Int | MultipleDie Int Die | OperationDie Die Operator | AddDie Die Die deriving Show
-- X | dX | Y[dice] | [dice][op] | [dice]+[dice]

type DiceCollection = [([Int], Int)]

fullCondenseDice :: DiceCollection -> DiceCollection
fullCondenseDice ds = map (\(x,y) -> ([x],y)) $ toList $ probs' ds

condenseDice :: DiceCollection -> DiceCollection
condenseDice ds = toList $ fromListWith (+) ds

uncondenseDice :: DiceCollection -> DiceCollection
uncondenseDice [] = []
uncondenseDice ((x,y):xs) = replicate y (x,1) ++ uncondenseDice xs

-- map of values to frequencies
probs' :: DiceCollection -> Map Int Int
probs' d = fromListWith (+) summed
    where summed = map (\(vals, count) -> (sum vals, count)) d

probs :: Die -> Map Int Int
probs d = probs' (expandDie d)

percentages :: Die -> Map Int Float
percentages d = M.map (\x -> 100 * (((/total) . fromIntegral) x)) probabilities
    where probabilities = probs d
          total = fromIntegral $ foldr (+) 0 probabilities

expected :: Die -> Float
expected d = (fromIntegral (sum $ map (\(x,y) -> x * y) (toList probabilities))) / total
    where probabilities = probs d
          total = fromIntegral $ foldr (+) 0 probabilities

combineWith :: ([Int] -> [Int] -> [Int]) -> DiceCollection -> DiceCollection -> DiceCollection
combineWith f [] _ = []
combineWith f ((x, y): xs) ys = map g ys ++ (combineWith f xs ys)
    where g (x', y') = (f x x', y * y')
        --   g _ = error "unexpanded dice in secondary collection"
-- combineWith _ _ _ = error "unexpanded dice in primary collection"

-- expandMult' :: DiceCollection -> DiceCollection -> DiceCollection
-- expandMult' [] _ = []
-- expandMult' ((x, 1):xs) ys = map (\(x', _) -> (x++x', 1)) ys ++ (expandMult' xs ys)
-- expandMult' _ _ = error "unexpanded dice detected"

expandMult :: Int -> DiceCollection -> DiceCollection
expandMult x xs
    | x == 0 = []
    | x == 1 = xs
    | x > 1 = combineWith (++) xs (expandMult (x-1) xs)
    | otherwise = error "negative expansion"

getOp :: Operator -> ([Int] -> [Int])
getOp (Max i) xs = map (min i) xs
getOp (Min i) xs = map (max i) xs
getOp (Keep High i) xs = take i $ sortOn (\x -> -x) xs
getOp (Keep Low i) xs = take i $ sort xs
getOp (Drop High i) xs = drop i $ sortOn (\x -> -x) xs
getOp (Drop Low i) xs = drop i $ sort xs

expandDie :: Die -> DiceCollection
expandDie (Const i) = [([i],1)]
expandDie (BaseDie i)
    | i > 0 = map (\x -> ([x], 1)) [1..i]
    | otherwise = error "die value cannot be less than 1"
expandDie (MultipleDie i d) = expandMult i $ expandDie d
-- expandDie (DieMod d i) = map (\(x,y) -> (map (+i) x, y)) (expandDie d)
expandDie (OperationDie d op) = condenseDice $ map (\(x,y) -> (dieOp x, y)) (expandDie d)
    where dieOp = getOp op
expandDie (AddDie d1 d2) = combineWith (\x y -> map (+ head y) x) d1' d2'
    where d1' = fullCondenseDice $ expandDie d1
          d2' = fullCondenseDice $ expandDie d2

d20 :: Die
d20 = BaseDie 20

roll2 :: Die -> Die
roll2 d = MultipleDie 2 d

adv :: Die
adv = OperationDie (MultipleDie 2 d20) (Keep High 1)

val = AddDie (OperationDie (MultipleDie 4 d20) (Keep High 1)) (Const 5)

-- highestOfLowest = OperationDie (MultipleDie 2 (OperationDie (MultipleDie 2 d20) (Keep Low 1))) (Keep High 1)
-- lowestOfHighest = OperationDie (MultipleDie 2 (OperationDie (MultipleDie 2 d20) (Keep High 1))) (Keep Low 1)
          

