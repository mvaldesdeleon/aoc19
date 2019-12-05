{-# LANGUAGE NamedFieldPuns #-}

module Day5
    ( day5
    ) where

import           Control.Monad
import           Data.Foldable       (traverse_)
import qualified Data.List           as L
import qualified Data.Vector.Mutable as V
import           Paths_aoc19         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-5.txt" >>= readFile

data ComputerState =
    ComputerState
        { memory :: V.IOVector Integer
        , ip     :: Integer
        , input  :: [Integer]
        , output :: [Integer]
        , halted :: Bool
        }

data Mode
    = Position
    | Immediate

parseMode :: Integer -> Mode
parseMode 0 = Position
parseMode 1 = Immediate

data Instruction =
    Instruction
        { opcode :: Integer
        , p1Mode :: Mode
        , p2Mode :: Mode
        , p3Mode :: Mode
        }

parseInput :: String -> IO ComputerState
parseInput string = do
    memory <- fromList (read $ "[" ++ string ++ "]")
    return $ ComputerState memory 0 [] [] False
  where
    fromList xs = do
        vector <- V.new (length xs)
        traverse_ (uncurry $ V.write vector) (zip [0 ..] xs)
        return vector

parseInstruction :: Integer -> Instruction
parseInstruction raw =
    let params = raw `div` 100
        p1 = params `mod` 10
        p2 = params `div` 10 `mod` 10
        p3 = params `div` 100
     in Instruction
            { opcode = raw `mod` 100
            , p1Mode = parseMode p1
            , p2Mode = parseMode p2
            , p3Mode = parseMode p3
            }

readParameters :: ComputerState -> Integer -> IO [Integer]
readParameters ComputerState {memory, ip} count =
    forM [1 .. count] (V.read memory . fromInteger . (+ ip))

applyMode :: ComputerState -> Mode -> Integer -> IO Integer
applyMode cs Immediate value = return value
applyMode ComputerState {memory} Position value =
    V.read memory (fromInteger value)

bit :: Bool -> Integer
bit True  = 1
bit False = 0

execute :: ComputerState -> IO ComputerState
execute cs@ComputerState {memory, ip, input = is, output = os} = do
    Instruction {opcode, p1Mode, p2Mode, p3Mode} <-
        parseInstruction <$> V.read memory (fromInteger ip)
    case opcode of
        1 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (firstV + secondV)
            return $ ComputerState memory (ip + 4) is os False
        2 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (firstV * secondV)
            return $ ComputerState memory (ip + 4) is os False
        3 -> do
            (first:_) <- readParameters cs 1
            V.write memory (fromInteger first) (head is)
            return $ ComputerState memory (ip + 2) (tail is) os False
        4 -> do
            (first:_) <- readParameters cs 1
            value <- V.read memory (fromInteger first)
            return $ ComputerState memory (ip + 2) is (value : os) False
        5 -> do
            (first:second:_) <- readParameters cs 2
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            return $
                ComputerState
                    memory
                    (if firstV /= 0
                         then secondV
                         else ip + 3)
                    is
                    os
                    False
        6 -> do
            (first:second:_) <- readParameters cs 2
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            return $
                ComputerState
                    memory
                    (if firstV == 0
                         then secondV
                         else ip + 3)
                    is
                    os
                    False
        7 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (bit (firstV < secondV))
            return $ ComputerState memory (ip + 4) is os False
        8 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (bit (firstV == secondV))
            return $ ComputerState memory (ip + 4) is os False
        99 -> return $ ComputerState memory ip is os True
        _ -> do
            print $ "invalid opcode: " ++ show opcode
            print $ "ip: " ++ show ip
            print $ "os: " ++ show os
            undefined

executeAll :: ComputerState -> IO ComputerState
executeAll cs = do
    ncs <- execute cs
    if halted ncs
        then return ncs
        else executeAll ncs

setInput :: [Integer] -> ComputerState -> IO ComputerState
setInput input cs = do
    newMemory <- V.clone (memory cs)
    return cs {input, memory = newMemory}

day5 :: IO ()
day5 = do
    cs <- loadInput >>= parseInput
    o1 <- head . output <$> (setInput [1] cs >>= executeAll)
    print o1
    o2 <- head . output <$> (setInput [5] cs >>= executeAll)
    print o2
