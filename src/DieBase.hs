module DieBase
    where

import Data.Map (Map, fromListWith, toList)
import qualified Data.Map as M
import Data.List as L (sort, sortOn)

data OperatorMod = Low | High deriving Show
data Operator = Keep OperatorMod Int | Drop OperatorMod Int | Min Int | Max Int deriving Show
data BinOp = BinOpLabelled (Int -> Int -> Int) String | BinOp (Int -> Int -> Int)

data Die = Const Int | BaseDie Int | MultipleDie Int Die | OperationDie Die Operator | BinaryOperatorDie BinOp Die Die deriving Show
-- X | dX | Y[dice] | [dice][op] | binOp [dice] [dice]

instance Show BinOp where
    show (BinOpLabelled _ label) = "(BinOpLabelled _ " ++ show label ++ ")"
    show (BinOp _) = "(BinOp _)"

getBinOp :: BinOp -> (Int -> Int -> Int)
getBinOp (BinOpLabelled f _) = f
getBinOp (BinOp f) = f

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
probs' die = fromListWith (+) summed
    where summed = map (\(vals, count) -> (sum vals, count)) die

probs :: Die -> Map Int Int
probs die = probs' (expandDie die)

percentages :: Die -> Map Int Float
percentages die = M.map (\x -> ((/total) . fromIntegral . (*100)) x) probabilities
    where probabilities = probs die
          total = fromIntegral $ foldr (+) 0 probabilities

expected :: Die -> Float
expected die = (fromIntegral (sum $ map (\(x,y) -> x * y) (toList probabilities))) / total
    where probabilities = probs die
          total = fromIntegral $ foldr (+) 0 probabilities

combineWith :: ([Int] -> [Int] -> [Int]) -> DiceCollection -> DiceCollection -> DiceCollection
combineWith _ [] _ = []
combineWith f ((x, y): xs) ys = map g ys ++ (combineWith f xs ys)
    where g (x', y') = (f x x', y * y')

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
expandDie (MultipleDie i die) = expandMult i $ expandDie die
expandDie (OperationDie die op) = condenseDice $ map (\(x,y) -> (dieOp x, y)) (expandDie die)
    where dieOp = getOp op
expandDie (BinaryOperatorDie b d1 d2) = combineWith (\x y -> [binOp (head x) (head y)]) d1' d2'
    where binOp = getBinOp b
          d1' = fullCondenseDice $ expandDie d1
          d2' = fullCondenseDice $ expandDie d2

d :: Int -> Die
d = BaseDie

(.*) :: Int -> Die -> Die
i .* die = MultipleDie i die

(.:) :: Die -> Operator -> Die
die .: op = OperationDie die op

d20 :: Die
d20 = d 20

adv :: Die
adv = OperationDie (2.*d20) (Keep High 1)

charGen :: Die
charGen = 4.*d 6 .: Keep High 3

-- highestOfLowest = OperationDie (MultipleDie 2 (OperationDie (MultipleDie 2 d20) (Keep Low 1))) (Keep High 1)
-- highestOfLowest = 2.*(2.*d20 .: Keep Low 1) .: Keep High 1
-- lowestOfHighest = OperationDie (MultipleDie 2 (OperationDie (MultipleDie 2 d20) (Keep High 1))) (Keep Low 1)
-- lowestOfHighest = 2.*(2.*d20 .: Keep High 1) .: Keep Low 1
          

