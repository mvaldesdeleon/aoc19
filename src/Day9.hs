module Day9
    ( day9
    ) where

import           Paths_aoc19 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-9.txt" >>= readFile

day9 :: IO ()
day9 = do
    undefined
