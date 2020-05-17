module Main where

-- import Lib
import DieBase

main :: IO ()
main = do
    putStrLn "d20"
    print $ stats d20
    putStrLn "\nadv"
    print $ stats adv
    putStrLn "\ncharGen"
    print $ stats charGen
    putStrLn "\nlevel 5 firebolt vs ac 15 (+4 mod)"
    print $ stats $ consAttack 7 15 (2.* d 10) 0
    putStrLn "\nlevel 5 fighter maul 2 attacks vs ac 15 (+4 mod)"
    print $ stats $ 2 .* consAttack 7 15 (2.* d 6) 4
