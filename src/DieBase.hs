module DieBase
    where

import Data.Map (Map, fromListWith, toList)
import qualified Data.Map
import Data.List as L (sort, sortOn)

data OperatorMod = Low | High deriving Show
data Reroll = Reroll (Int -> Bool)
data BinOp = BinOp { binOp :: (Int -> Int -> Int) }
data GenOp = GenOp { genOp :: [Int] -> [Int] }
data Operator = OperatorKeep OperatorMod Int -- high or low, and how many
              | OperatorDrop OperatorMod Int -- high or low, and how many
              | OperatorMin Int -- minimum value allowed
              | OperatorMax Int -- minimum value allowed
              | OperatorGeneric GenOp -- to allow generic calculations (not recommended)
              | OperatorThreshold Int -- to threshold values below a certain value
            --   | OperatorAttack Int Int Die -- AC of target, minimum value of a crit attack, and dice to be added on a crit
              deriving Show

data Die = Const Int 
         | CustomDie [(Int, Int)] -- value to frequency
         | BaseDie Int 
         | MultipleDie Int Die 
         | OperationDie Die Operator 
         | BinaryOperatorDie BinOp Die Die 
         | RerollDie Die Reroll 
         | AttackDie Die Int Int Die Int Die -- hitting die, val to equal or exceed, always fail threshold, val of damage, minimum crit value, total val of crit damage
         deriving Show
-- X | dX | Y[dice] | [dice][op] | binOp [dice] [dice] | [dice][reroll on]

instance Show BinOp where
    show (BinOp _) = "BinOp"
instance Show GenOp where
    show (GenOp _) = "GenOp"
instance Show Reroll where
    show (Reroll _) = "Reroll"

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
percentages die = Data.Map.map (\x -> ((/total) . fromIntegral . (*100)) x) probabilities
    where probabilities = probs die
          total = fromIntegral $ foldr (+) 0 probabilities

expected :: Die -> Float
expected die = (fromIntegral (sum $ map (\(x,y) -> x * y) (toList probabilities))) / total
    where probabilities = probs die
          total = fromIntegral $ foldr (+) 0 probabilities

stats :: Die -> (Float, Map Int Float)
stats die = (expected die, percentages die)

totalFreq' :: DiceCollection -> Int
totalFreq' xs = foldr (\x y -> snd x + y) 0 xs

totalFreq :: Die -> Int
totalFreq die = totalFreq' (expandDie die)

combineWith :: ([Int] -> [Int] -> [Int]) -> (Int -> Int -> Int) -> DiceCollection -> DiceCollection -> DiceCollection
combineWith _ _ [] _ = []
combineWith f f' ((x, y): xs) ys = map g ys ++ (combineWith f f' xs ys)
    where g (x', y') = (f x x', f' y y')

expandMult :: Int -> DiceCollection -> DiceCollection
expandMult x xs
    | x == 0 = []
    | x == 1 = xs
    | x > 1 = combineWith (++) (*) xs (expandMult (x-1) xs)
    | otherwise = error "negative expansion"

getOp :: Operator -> ([Int] -> [Int])
getOp (OperatorMax i) xs = map (min i) xs
getOp (OperatorMin i) xs = map (max i) xs
getOp (OperatorKeep High i) xs = take i $ sortOn (\x -> -x) xs
getOp (OperatorKeep Low i) xs = take i $ sort xs
getOp (OperatorDrop High i) xs = drop i $ sortOn (\x -> -x) xs
getOp (OperatorDrop Low i) xs = drop i $ sort xs
getOp (OperatorGeneric (GenOp f)) xs = f xs
getOp (OperatorThreshold i) xs
    | sum xs < i = []
    | otherwise = [1]

replaceIf' :: (Int -> Bool) -> DiceCollection -> DiceCollection -> DiceCollection
replaceIf' _ [] _ = []
replaceIf' f ((x,y):xs) ys
    | f (sum x) = concat (replicate y ys) ++ replaceIf' f xs ys
    | otherwise = (x,y): replaceIf' f xs ys

replaceIf :: (Int -> Bool) -> DiceCollection -> DiceCollection
replaceIf f xs = replaceIf' f xs xs

expandBinOp :: (Int -> Int -> Int) -> Die -> Die -> (Int -> Int -> Int) -> DiceCollection
expandBinOp b die1 die2 yFunc = combineWith (\x y -> [b (head x) (head y)]) yFunc die1' die2'
    where die1' = fullCondenseDice $ expandDie die1
          die2' = fullCondenseDice $ expandDie die2

expandAttack :: DiceCollection -> Int -> Int -> Die -> Int -> Die -> DiceCollection
expandAttack [] _ _ _ _ _ = []
expandAttack ((x,y):xs) threshold miss dmg critThreshold critDmg
    | sum x < threshold = (expandDie (dmg ..* CustomDie [(0,y)] ..* nonCritAdjust)) ++ expandAttack xs threshold miss dmg critThreshold critDmg
    | sum x >= critThreshold = expandDie ((dmg ..+ critDmg) ..* hits) ++ expandAttack xs threshold miss dmg critThreshold critDmg
    | sum x <= miss = (expandDie (dmg ..* CustomDie [(0,y)] ..* nonCritAdjust)) ++ expandAttack xs threshold miss dmg critThreshold critDmg
    | otherwise = expandDie (damage ..* nonCritAdjust) ++ expandAttack xs threshold miss dmg critThreshold critDmg
    where hits = CustomDie [(1,y)]
          nonCritAdjust = CustomDie [(1,totalFreq critDmg)]
          damage = dmg ..* hits

expandDie :: Die -> DiceCollection
expandDie (Const i) = [([i],1)]
expandDie (CustomDie xs) = map (\(x,y) -> ([x],y)) xs
expandDie (BaseDie i)
    | i > 0 = map (\x -> ([x], 1)) [1..i]
    | otherwise = error "die value cannot be less than 1"
expandDie (MultipleDie i die) = expandMult i $ expandDie die
expandDie (OperationDie die op) = condenseDice $ map (\(x,y) -> (dieOp x, y)) (expandDie die)
    where dieOp = getOp op
expandDie (BinaryOperatorDie (BinOp b) die1 die2) = expandBinOp b die1 die2 (*)
expandDie (RerollDie die (Reroll reroll)) = condenseDice $ replaceIf reroll die'
    where die' = expandDie die
expandDie (AttackDie toHit threshold miss dmg critThreshold critDmg) = expandAttack (expandDie toHit) threshold miss dmg critThreshold critDmg

d :: Int -> Die
d = BaseDie

kp :: OperatorMod -> Int -> Operator
kp = OperatorKeep
dp :: OperatorMod -> Int -> Operator
dp = OperatorDrop
h :: OperatorMod
h = High
l :: OperatorMod
l = Low

-- repeat die multiple times
(.*) :: Int -> Die -> Die
i .* die = MultipleDie i die

-- add an operator to some dice
(.:) :: Die -> Operator -> Die
die .: op = OperationDie die op

-- multiply two dice together
(..*) :: Die -> Die -> Die
die1 ..* die2 = BinaryOperatorDie (BinOp (*)) die1 die2
-- add two dice together
(..+) :: Die -> Die -> Die
die1 ..+ die2 = BinaryOperatorDie (BinOp (+)) die1 die2

-- reroll on a given function
(.#) :: Die -> (Int -> Bool) -> Die
die1 .# f = RerollDie die1 (Reroll f)

d20 :: Die
d20 = d 20

adv :: Die
adv = (2.*d20).:kp h 1

charGen :: Die
charGen = 4.*d 6 .: OperatorKeep High 3

consAttack :: Int -> Int -> Die -> Int -> Die
consAttack modifier ac dmg dmgMod = AttackDie (d20 ..+ Const modifier) ac (1 + modifier) (dmg ..+ Const dmgMod) (20 + modifier) dmg

baseMaul :: Die
baseMaul = 2.*(d 6.#(<3))
maulDamage :: Die
maulDamage = baseMaul ..+ Const 3
maulAttack :: Die
maulAttack = AttackDie (d20 ..+ Const 4) 13 6 maulDamage 24 baseMaul

-- highestOfLowest = OperationDie (MultipleDie 2 (OperationDie (MultipleDie 2 d20) (OperatorKeep Low 1))) (OperatorKeep High 1)
-- highestOfLowest = 2.*(2.*d20 .: OperatorKeep Low 1) .: OperatorKeep High 1
-- lowestOfHighest = OperationDie (MultipleDie 2 (OperationDie (MultipleDie 2 d20) (OperatorKeep High 1))) (OperatorKeep Low 1)
-- lowestOfHighest = 2.*(2.*d20 .: OperatorKeep High 1) .: OperatorKeep Low 1
          

