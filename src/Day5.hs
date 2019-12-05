module Day5
    ( day5
    ) where

import           Paths_aoc19 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-5.txt" >>= readFile

day5 :: IO ()
day5 = do
    undefined
