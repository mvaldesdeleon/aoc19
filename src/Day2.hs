module Day2
    ( day2
    ) where

import qualified Data.List   as L
import           Paths_aoc19 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-2.txt" >>= readFile

data ComputerState =
    ComputerState
        { memory :: [Integer]
        , ip     :: Integer
        }
    deriving (Show)

parseInput :: String -> ComputerState
parseInput string = ComputerState (read $ "[" ++ string ++ "]") 0

readOperands :: ComputerState -> (Integer, Integer, Integer)
readOperands (ComputerState memory ip) =
    let first = memory `L.genericIndex` (memory `L.genericIndex` (ip + 1))
        second = memory `L.genericIndex` (memory `L.genericIndex` (ip + 2))
        third = memory `L.genericIndex` (ip + 3)
     in (first, second, third)

execute :: ComputerState -> (ComputerState, Bool)
execute cs@(ComputerState memory ip) =
    let current = memory `L.genericIndex` ip
     in case current of
            1 ->
                let (first, second, third) = readOperands cs
                    result = first + second
                    newMemory = update memory third result
                 in (ComputerState newMemory (ip + 4), False)
            2 ->
                let (first, second, third) = readOperands cs
                    result = first * second
                    newMemory = update memory third result
                 in (ComputerState newMemory (ip + 4), False)
            99 -> (cs, True)
            _ -> undefined

update :: [a] -> Integer -> a -> [a]
update list position value =
    let (prefix, suffix) = L.genericSplitAt position list
     in prefix ++ [value] ++ drop 1 suffix

initialize :: (Integer, Integer) -> ComputerState -> ComputerState
initialize (noun, verb) (ComputerState memory ip) =
    let firstChange = update memory 1 noun
        secondChange = update firstChange 2 verb
     in ComputerState secondChange ip

executeAll :: ComputerState -> ComputerState
executeAll cs =
    let (ncs, complete) = execute cs
     in if complete
            then ncs
            else executeAll ncs

readMemory :: Integer -> ComputerState -> Integer
readMemory position cs = memory cs `L.genericIndex` position

test :: (Integer, Integer) -> ComputerState -> Integer
test (noun, verb) = readMemory 0 . executeAll . initialize (noun, verb)

allValues :: ComputerState -> [(Integer, Integer, Integer)]
allValues cs =
    [(test (noun, verb) cs, noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

find :: ComputerState -> Integer
find cs =
    let (_, noun, verb) = head $ filter ((==) 19690720 . value) $ allValues cs
     in noun * 100 + verb
  where
    value (val, noun, verb) = val

day2 :: IO ()
day2 = do
    cs <- parseInput <$> loadInput
    print $ test (12, 2) cs
    print $ find cs
