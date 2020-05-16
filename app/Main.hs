module Main where

-- import Lib
import DieBase

main :: IO ()
main = do
    print $ percentages d20
    print $ expected d20
    print $ percentages adv
    print $ expected adv
