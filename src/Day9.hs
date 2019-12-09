{-# LANGUAGE NamedFieldPuns #-}

module Day9
    ( day9
    ) where

import           Control.Monad
import           Data.Foldable       (traverse_)
import           Data.List           (permutations, reverse)
import           Data.Maybe          (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Vector.Mutable as V
import           Paths_aoc19         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-9.txt" >>= readFile

data ExecutionState
    = Ready
    | WaitingInput
    | Halted
    deriving (Show, Eq)

data ComputerState =
    ComputerState
        { memory         :: V.IOVector Integer
        , ip             :: Integer
        , rb             :: Integer
        , input          :: [Integer]
        , output         :: [Integer]
        , executionState :: ExecutionState
        }

parseInput :: String -> IO ComputerState
parseInput string = do
    memory <- fromList (read $ "[" ++ string ++ "]")
    return $ ComputerState memory 0 0 [] [] Ready
  where
    fromList xs = do
        let ys = xs ++ take (9 * length xs) (repeat 0)
        vector <- V.new (length ys)
        traverse_ (uncurry $ V.write vector) (zip [0 ..] ys)
        return vector

data Mode
    = Position
    | Immediate
    | Relative
    deriving (Show)

parseMode :: Integer -> Mode
parseMode 0 = Position
parseMode 1 = Immediate
parseMode 2 = Relative

data Instruction =
    Instruction
        { opcode :: Integer
        , p1Mode :: Mode
        , p2Mode :: Mode
        , p3Mode :: Mode
        }
    deriving (Show)

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

readV :: V.IOVector a -> Integer -> IO a
readV v = V.read v . fromInteger

writeV :: V.IOVector a -> Integer -> a -> IO ()
writeV v i = V.write v (fromInteger i)

readParameters :: ComputerState -> Integer -> IO [Integer]
readParameters ComputerState {memory, ip} count =
    forM [1 .. count] (readV memory . (+ ip))

applyMode :: ComputerState -> Mode -> Integer -> IO Integer
applyMode _ Immediate value = return value
applyMode ComputerState {memory} Position value = readV memory value
applyMode ComputerState {memory, rb} Relative value = readV memory (rb + value)

applyModeWrite :: ComputerState -> Mode -> Integer -> IO Integer
applyModeWrite _ Immediate _ = error "Attempting to write under Immediate mode"
applyModeWrite _ Position value = return value
applyModeWrite ComputerState {rb} Relative value = return $ value + rb

bit :: Bool -> Integer
bit True  = 1
bit False = 0

execute :: ComputerState -> IO ComputerState
execute cs@ComputerState { memory
                         , ip
                         , rb
                         , input = is
                         , output = os
                         , executionState = es
                         } = do
    when (es == Halted) $ error "Attempting to execute from a Halted state"
    Instruction {opcode, p1Mode, p2Mode, p3Mode} <-
        parseInstruction <$> readV memory ip
    case opcode of
        1 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            thirdV <- applyModeWrite cs p3Mode third
            writeV memory thirdV (firstV + secondV)
            return $ ComputerState memory (ip + 4) rb is os Ready
        2 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            thirdV <- applyModeWrite cs p3Mode third
            writeV memory thirdV (firstV * secondV)
            return $ ComputerState memory (ip + 4) rb is os Ready
        3 -> do
            (first:_) <- readParameters cs 1
            firstV <- applyModeWrite cs p1Mode first
            case is of
                [] -> return $ ComputerState memory ip rb is os WaitingInput
                (i:is) -> do
                    writeV memory firstV i
                    return $ ComputerState memory (ip + 2) rb is os Ready
        4 -> do
            (first:_) <- readParameters cs 1
            firstV <- applyMode cs p1Mode first
            return $ ComputerState memory (ip + 2) rb is (firstV : os) Ready
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
                    rb
                    is
                    os
                    Ready
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
                    rb
                    is
                    os
                    Ready
        7 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            thirdV <- applyModeWrite cs p3Mode third
            writeV memory thirdV (bit (firstV < secondV))
            return $ ComputerState memory (ip + 4) rb is os Ready
        8 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            thirdV <- applyModeWrite cs p3Mode third
            writeV memory thirdV (bit (firstV == secondV))
            return $ ComputerState memory (ip + 4) rb is os Ready
        9 -> do
            (first:_) <- readParameters cs 1
            firstV <- applyMode cs p1Mode first
            return $ ComputerState memory (ip + 2) (rb + firstV) is os Ready
        99 -> return $ ComputerState memory ip rb is os Halted
        _ -> error $ "Invalid Opcode " ++ show opcode

executeAll :: ComputerState -> IO ComputerState
executeAll cs = do
    ncs <- execute cs
    if executionState ncs == Ready
        then executeAll ncs
        else return ncs

clone :: ComputerState -> IO ComputerState
clone cs = do
    newMemory <- V.clone (memory cs)
    return cs {memory = newMemory}

setInput :: [Integer] -> ComputerState -> ComputerState
setInput input cs = cs {input}

day9 :: IO ()
day9 = do
    cs <- loadInput >>= parseInput
    keycode <- clone cs >>= executeAll . setInput [1]
    print $ head . output $ keycode
    coords <- clone cs >>= executeAll . setInput [2]
    print $ head . output $ coords
