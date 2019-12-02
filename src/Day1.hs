module Day1
    ( day1
    ) where

import           Paths_aoc19 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-1.txt" >>= readFile

data Module =
    Module
        { mass :: Integer
        , fuel :: Integer
        }

fuelRequired :: Integer -> Integer
fuelRequired mass = mass `div` 3 - 2

parseInput :: String -> [Module]
parseInput = map readModule . lines
  where
    readModule str =
        let mass = read str
         in Module mass (fuelRequired mass)

-- Part 1
fuelForModules :: [Module] -> Integer
fuelForModules = sum . map fuel

-- Part 2
fuelForFuel :: Integer -> Integer
fuelForFuel = sum . takeWhile (> 0) . iterate fuelRequired

fuelForFueledModules :: [Module] -> Integer
fuelForFueledModules = sum . map (fuelForFuel . fuel)

day1 :: IO ()
day1 = do
    modules <- parseInput <$> loadInput
    print $ fuelForModules modules
    print $ fuelForFueledModules modules
