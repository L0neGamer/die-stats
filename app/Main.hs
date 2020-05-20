module Main where

-- import Lib
import DieBase

main :: IO ()
main = do
    -- putStrLn "d20"
    -- print $ stats d20
    -- putStrLn "\nadv"
    -- print $ stats adv
    -- print $ atMost adv
    -- putStrLn "\ncharGen"
    -- print $ stats charGen
    -- putStrLn "\nlevel 9 firebolt vs ac 15 (+5 mod)"
    -- print $ stats $ consAttack 9 15 (2.* d 10) 0
    -- putStrLn "\nlevel 9 fighter maul 2 attacks vs ac 15 (+5 mod)"
    -- print $ stats $ 2 .* consAttack 9 15 (2.* d 6) 5
    -- putStrLn "\nlevel 9 fighter maul 2 attacks reroll 1s and 2s vs ac 15 (+5 mod)"
    -- print $ stats $ 2 .* consAttack 9 15 (2.* d 6 .# (<3)) 5
    print $ stats $ 4 .* d 100
