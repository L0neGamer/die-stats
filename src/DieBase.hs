-- {-# BangPatterns  #}

module DieBase
    where

import Data.Map (Map, fromListWith, toAscList, toDescList, fromList)
import qualified Data.Map
import Data.Ratio
import Data.SortedList as SL hiding (map)
import qualified Data.SortedList as SL (map)
import Data.Bifunctor (first)

data OperatorMod = Low | High deriving Show
newtype Reroll = Reroll (Integer -> Bool)
newtype BinOp = BinOp { binOp :: Integer -> Integer -> Integer }
newtype GenOp = GenOp { genOp :: DiceSet -> DiceSet }
data Operator = OperatorKeep OperatorMod Integer -- high or low, and how many
              | OperatorDrop OperatorMod Integer -- high or low, and how many
              | OperatorMin Integer -- minimum value allowed
              | OperatorMax Integer -- minimum value allowed
              | OperatorGeneric GenOp -- to allow generic calculations (not recommended)
              | OperatorThreshold Integer -- to threshold values below a certain value
              deriving Show

data Die = Const Integer                    -- a constant, example use being adding to a set of dice
         | CustomDie [(Integer, DiceProb)]  -- value to frequency - use to make custom dice
         | BaseDie Integer                  -- the base value of a die
         | MultipleDie Integer Die          -- roll multiple dice at the same time, combine results
         | OperationDie Die Operator        -- for things that only operate on a single set of dice (keep, drop, min, max, threshold the die)
         | BinaryOperatorDie BinOp Die Die  -- for doing operations that combine two die results (condenses both!)
         | RerollDie Die Reroll             -- reroll a die when the function is true
         | AttackDie Die Integer Integer Die Integer Die -- hitting die, val to equal or exceed, always fail threshold, val of damage, minimum crit value, total val of crit damage
         deriving Show
-- X | dX | Y[dice] | [dice][op] | binOp [dice] [dice] | [dice][reroll on] | no idea how to write this

instance Show BinOp where
    show (BinOp _) = "BinOp"
instance Show GenOp where
    show (GenOp _) = "GenOp"
instance Show Reroll where
    show (Reroll _) = "Reroll"

type DiceProb = Ratio Integer
type DiceSet = SortedList Integer
type DiceCollection = [(DiceSet, DiceProb)]

fromBool :: Num a => Bool -> a
fromBool = fromIntegral . fromEnum

-- converts a diceprob to its percentage
toPercent :: DiceProb -> Float
toPercent = fromRational . (* 100)

-- condense everything down to values
fullCondenseDice :: DiceCollection -> DiceCollection
fullCondenseDice = fmap (first singleton) . toAscList . probs'

-- condense similar dice sets
condenseDice :: DiceCollection -> DiceCollection
condenseDice = toAscList . fromListWith (+)

condenseIf :: Bool -> DiceCollection -> DiceCollection
condenseIf True = fullCondenseDice
condenseIf _    = condenseDice

-- from a dicecollection, get a map of values to frequencies
probs' :: DiceCollection -> Map Integer DiceProb
probs' = fromListWith (+) . map (first sum)

-- expand a die then call probs'
probs :: Die -> Map Integer DiceProb
probs = probs' . expandDie

-- get the percentages of each value from a die
percentages'' :: Map Integer DiceProb -> Map Integer Float
percentages'' = Data.Map.map toPercent
percentages' :: DiceCollection -> Map Integer Float
percentages' = percentages'' . probs'
percentages :: Die -> Map Integer Float
percentages = percentages' . expandDie

-- what is the expected value of a die
expected'' :: Map Integer DiceProb -> Float
expected'' = fromRational . sum . map (\(x,y) -> fromIntegral x * y) . toAscList
expected' :: DiceCollection -> Float
expected' = expected'' . probs'
expected :: Die -> Float
expected = expected' . expandDie

-- get the expected value and the percentages for a die
stats'' :: Map Integer DiceProb -> (Float, Map Integer Float)
stats'' mp = (expected'' mp, percentages'' mp)
stats' :: DiceCollection -> (Float, Map Integer Float)
stats' = stats'' . probs'
stats :: Die -> (Float, Map Integer Float)
stats = stats' . expandDie

-- given a list of items and values, accumulate the values
accumulate :: Num a => a -> [(b, a)] -> [(b, a)]
accumulate _ [] = []
accumulate v ((x, y):xs) = (x, v + y) : accumulate (v + y) xs

-- given a way to convert a map into a list and a dice collection, return a map of values to the probabilities of each value but accumulated with the previous value
accumulateProbability :: (Map Integer DiceProb -> [(Integer, DiceProb)]) -> DiceCollection -> Map Integer Float
accumulateProbability toXList = Data.Map.map toPercent . fromList . accumulate 0 . toXList . probs'

-- get the probability of getting at most (or least) this item
atMost :: Die -> Map Integer Float
atMost = accumulateProbability toAscList . expandDie 
atLeast :: Die -> Map Integer Float
atLeast = accumulateProbability toDescList . expandDie

-- combine two dice collections by mapping a combination of one over the other
combineWith :: (DiceSet -> DiceSet -> DiceSet)-> DiceCollection -> DiceCollection -> DiceCollection
combineWith _ [] _ = []
combineWith f ((x, y): xs) ys = map g ys <> combineWith f xs ys
    where g (x', y') = (f x x', y * y')

-- repeat and combine a dicecollection with itself
expandMult :: Bool -> Integer -> DiceCollection -> DiceCollection
expandMult b x xs
    | x == 0 = []
    | x == 1 = xs
    | x > 1 = combineWith mappend (condenseIf b $ expandMult b (x-1) xs) xs
    | otherwise = error "negative expansion"

-- given an operator, return a function that takes a list of ints and returns a list of ints
getOp :: Operator -> (DiceSet -> DiceSet)
getOp (OperatorMax i) = SL.map (min i)
getOp (OperatorMin i) = SL.map (max i)
getOp (OperatorKeep High i) = SL.reverseDown . SL.take (fromInteger i) . SL.reverse
getOp (OperatorKeep Low i)  = SL.take (fromInteger i)
getOp (OperatorDrop High i) = SL.reverseDown . SL.drop (fromInteger i) . SL.reverse
getOp (OperatorDrop Low i)  = SL.drop (fromInteger i)
getOp (OperatorGeneric (GenOp f)) = f
getOp (OperatorThreshold i) = singleton . fromBool . (>= i) . sum

-- replace a collection of values if their sum meets some criteria with a second list. else, continue
replaceIf' :: (Integer -> Bool) -> DiceCollection -> DiceCollection -> DiceCollection
replaceIf' _ [] _ = []
replaceIf' f ((x,y):xs) ys
    | f (sum x) = map (\(x',y') -> (x', y' * y)) ys ++ replaceIf' f xs ys
    | otherwise = (x, y): replaceIf' f xs ys

-- call replaceIf' on the same list twice
replaceIf :: (Integer -> Bool) -> DiceCollection -> DiceCollection
replaceIf f xs = replaceIf' f xs xs

-- condenses two dice, and then combines them according to a binary operator
expandBinOp :: (Integer -> Integer -> Integer) -> Die -> Die -> DiceCollection
expandBinOp b die1 die2 = combineWith (\x y -> singleton $ b (head' x) (head' y)) die1' die2'
    where die1' = fullCondenseDice . expandDie $ die1
          die2' = fullCondenseDice . expandDie $ die2
          head' xs = head $ fromSortedList xs

-- do some attack calculations
expandAttack :: DiceCollection -> Integer -> Integer -> Die -> Integer -> Die -> DiceCollection
expandAttack [] _ _ _ _ _ = []
expandAttack ((x,y):xs) threshold miss dmg critThreshold critDmg
    | sum x >= critThreshold              = expandDie ((dmg ..+ critDmg) ..* hits True) ++ nextVal
    | sum x <  threshold || sum x <= miss = expandDie (dmg ..* hits False)              ++ nextVal
    | otherwise                           = expandDie (dmg ..* hits True)               ++ nextVal
    where hits b = CustomDie [(fromBool b,y)]
          nextVal = expandAttack xs threshold miss dmg critThreshold critDmg

-- expand a die and give a dice collection from it
expandDie' :: Bool -> Die -> DiceCollection
expandDie' _ (Const i)      = [(singleton i, 1)]
expandDie' _ (CustomDie xs) = map (first singleton) xs
expandDie' _ (BaseDie i)
    | i > 0 = map (\x -> (singleton x, 1 / fromIntegral i)) [1..i]
    | otherwise = error "die value cannot be less than 1"
expandDie' b (MultipleDie i die)  = condenseIf b $ expandMult b i $ expandDie' b die
expandDie' b (OperationDie die op) = condenseIf b $ map (first dieOp) (expandDie' False die)
    where dieOp = getOp op
expandDie' _ (BinaryOperatorDie (BinOp bOp) die1 die2) = expandBinOp bOp die1 die2
expandDie' _ (RerollDie die (Reroll reroll)) = condenseDice . replaceIf reroll . expandDie' True $ die
expandDie' _ (AttackDie toHit threshold miss dmg critThreshold critDmg) = fullCondenseDice $ expandAttack (expandDie' True toHit) threshold miss dmg critThreshold critDmg

expandDie :: Die -> DiceCollection
expandDie = expandDie' True

-- easy constructor for an N sided die; d 6 -> a cube die
d :: Integer -> Die
d = BaseDie

-- easy constructors for keep and drop operators, as well as their modifiers of high and low
kp :: OperatorMod -> Integer -> Operator
kp = OperatorKeep
dp :: OperatorMod -> Integer -> Operator
dp = OperatorDrop
h :: OperatorMod
h = High
l :: OperatorMod
l = Low

-- repeat a die multiple times; 2 (die) -> roll die twice and match each value with every value
(.*) :: Integer -> Die -> Die
i .* die = MultipleDie i die

-- add an operator to some dice; add an operator onto a die
(.:) :: Die -> Operator -> Die
die .: op = OperationDie die op

-- multiply two dice together
(..*) :: Die -> Die -> Die
die1 ..* die2 = BinaryOperatorDie (BinOp (*)) die1 die2
-- add two dice together
(..+) :: Die -> Die -> Die
die1 ..+ die2 = BinaryOperatorDie (BinOp (+)) die1 die2

-- reroll on a given function
(.#) :: Die -> (Integer -> Bool) -> Die
die1 .# f = RerollDie die1 (Reroll f)

-- value is or is greater than threshold
(.>) :: Die -> Integer -> Die
die .> i = OperationDie die (OperatorThreshold i)

d20 :: Die
d20 = d 20

adv :: Die
adv = (2.*d20).:kp h 1

charGen :: Die
charGen = 4.*d 6 .:kp h 3

consAttack :: Integer -> Integer -> Die -> Integer -> Die
consAttack modifier ac dmg dmgMod = AttackDie (d20 ..+ Const modifier) ac (1 + modifier) (dmg ..+ Const dmgMod) (20 + modifier) dmg
