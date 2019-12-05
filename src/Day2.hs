module Day2
    ( day2
    ) where

import           Data.Foldable       (traverse_)
import qualified Data.List           as L
import qualified Data.Vector.Mutable as V
import           Paths_aoc19         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-2.txt" >>= readFile

data ComputerState =
    ComputerState
        { memory :: V.IOVector Integer
        , ip     :: Integer
        , halted :: Bool
        }

parseInput :: String -> IO ComputerState
parseInput string = do
    memory <- fromList (read $ "[" ++ string ++ "]")
    return $ ComputerState memory 0 False
  where
    fromList xs = do
        vector <- V.new (length xs)
        traverse_ (uncurry $ V.write vector) (zip [0 ..] xs)
        return vector

readOperands :: ComputerState -> IO (Integer, Integer, Integer)
readOperands (ComputerState memory ip _) = do
    let pointer = fromInteger ip
    first <- V.read memory (pointer + 1) >>= V.read memory . fromInteger
    second <- V.read memory (pointer + 2) >>= V.read memory . fromInteger
    third <- V.read memory (pointer + 3)
    return (first, second, third)

execute :: ComputerState -> IO ComputerState
execute cs@(ComputerState memory ip _) = do
    current <- V.read memory (fromInteger ip)
    case current of
        1 -> do
            (first, second, third) <- readOperands cs
            V.write memory (fromInteger third) (first + second)
            return $ ComputerState memory (ip + 4) False
        2 -> do
            (first, second, third) <- readOperands cs
            V.write memory (fromInteger third) (first * second)
            return $ ComputerState memory (ip + 4) False
        99 -> return $ ComputerState memory ip True
        _ -> undefined

initialize :: (Integer, Integer) -> ComputerState -> IO ComputerState
initialize (noun, verb) (ComputerState memory ip halted) = do
    newMemory <- V.clone memory
    V.write newMemory 1 noun
    V.write newMemory 2 verb
    return $ ComputerState newMemory ip halted

executeAll :: ComputerState -> IO ComputerState
executeAll cs = do
    ncs <- execute cs
    if halted ncs
        then return ncs
        else executeAll ncs

readMemory :: Integer -> ComputerState -> IO Integer
readMemory position cs = V.read (memory cs) (fromInteger position)

test :: (Integer, Integer) -> ComputerState -> IO Integer
test (noun, verb) cs =
    initialize (noun, verb) cs >>= executeAll >>= readMemory 0

allValues :: ComputerState -> IO [(Integer, Integer, Integer)]
allValues cs = traverse fn [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
  where
    fn (noun, verb) = do
        value <- test (noun, verb) cs
        return (value, noun, verb)

find :: ComputerState -> IO Integer
find cs = do
    (_, noun, verb) <- head . filter ((==) 19690720 . value) <$> allValues cs
    return $ noun * 100 + verb
  where
    value (val, noun, verb) = val

day2 :: IO ()
day2 = do
    cs <- loadInput >>= parseInput
    print =<< test (12, 2) cs
    print =<< find cs
