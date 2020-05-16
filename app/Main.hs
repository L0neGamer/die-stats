module Main where

-- import Lib
import DieBase

main :: IO ()
main = do
    putStrLn "d20"
    print $ percentages d20
    print $ expected d20
    putStrLn "\nadv"
    print $ percentages adv
    print $ expected adv
    putStrLn "\ncharGen"
    print $ percentages charGen
    print $ expected charGen
