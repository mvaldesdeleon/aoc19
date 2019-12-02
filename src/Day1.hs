module Day1
    ( day1
    ) where

import           Paths_aoc19 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-1.txt" >>= readFile

type Mass = Integer

type Fuel = Integer

parseInput :: String -> [Mass]
parseInput = map read . lines

-- Part 1
fuelRequired :: Mass -> Fuel
fuelRequired mass = mass `div` 3 - 2

totalFuelRequired :: [Mass] -> Fuel
totalFuelRequired = foldl step 0
  where
    step fuel mass = fuel + fuelRequired mass

-- Part 2
fuelMass :: Fuel -> Mass
fuelMass = id

fuelMassRequired :: Mass -> Mass
fuelMassRequired = fuelMass . fuelRequired

completeFuelMassRequired :: Mass -> Mass
completeFuelMassRequired =
    sum . takeWhile (> 0) . drop 1 . iterate fuelMassRequired

totalFuelMassRequired :: [Mass] -> Mass
totalFuelMassRequired = sum . map completeFuelMassRequired

day1 :: IO ()
day1 = do
    masses <- parseInput <$> loadInput
    print $ totalFuelRequired masses
    print $ totalFuelMassRequired masses
