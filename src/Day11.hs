{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Day11
    ( day11
    ) where

import           Control.Monad
import           Data.Foldable       (traverse_)
import           Data.List           (permutations, reverse)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Vector.Mutable as V
import           Paths_aoc19         (getDataFileName)
import           Prelude             hiding (Left, Right)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-11.txt" >>= readFile

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
        let ys = xs ++ replicate (9 * length xs) 0
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

data Color
    = Black
    | White
    deriving (Show, Eq)

colorIndex :: Color -> Integer
colorIndex Black = 0
colorIndex White = 1

parseColor :: Integer -> Color
parseColor 0 = Black
parseColor 1 = White

data Direction
    = Up
    | Right
    | Down
    | Left
    deriving (Show, Eq, Enum, Bounded)

cw :: Direction -> Direction
cw d
    | d == maxBound = minBound
    | otherwise = succ d

ccw :: Direction -> Direction
ccw d
    | d == minBound = maxBound
    | otherwise = pred d

move :: Direction -> (Integer, Integer) -> (Integer, Integer)
move Up (x, y)    = (x, y + 1)
move Right (x, y) = (x + 1, y)
move Down (x, y)  = (x, y - 1)
move Left (x, y)  = (x - 1, y)

data State =
    State
        { cs        :: ComputerState
        , position  :: (Integer, Integer)
        , direction :: Direction
        , canvas    :: M.Map (Integer, Integer) Color
        }

initState :: Color -> ComputerState -> State
initState color cs =
    let origin = (0, 0)
     in State
            { cs
            , position = origin
            , direction = Up
            , canvas = M.singleton origin color
            }

printStep :: State -> IO State
printStep State {..} = do
    let i = colorIndex $ M.findWithDefault Black position canvas
    csNext <- executeAll $ setInput [i] cs
    let [rotation, color] = output csNext
        directionNext =
            if rotation == 0
                then ccw direction
                else cw direction
        positionNext = move directionNext position
    return
        State
            { cs = csNext {output = []}
            , position = positionNext
            , direction = directionNext
            , canvas = M.insert position (parseColor color) canvas
            }

paintAll :: State -> IO State
paintAll s = do
    ns <- printStep s
    if executionState (cs ns) == Halted
        then return ns
        else paintAll ns

printCanvas :: M.Map (Integer, Integer) Color -> String
printCanvas canvas =
    let keys = M.keys canvas
        bottom = minimum . map snd $ keys
        left = minimum . map fst $ keys
        top = maximum . map snd $ keys
        right = maximum . map fst $ keys
     in unlines [printRow left right y | y <- [top,top - 1 .. bottom]]
  where
    printRow left right y =
        [ printColor $ M.findWithDefault Black (x, y) canvas
        | x <- [left .. right]
        ]
    printColor Black = ' '
    printColor White = '#'

day11 :: IO ()
day11 = do
    cs <- loadInput >>= parseInput
    fs <- initState Black <$> clone cs >>= paintAll
    print $ length . M.keys . canvas $ fs
    fs <- initState White <$> clone cs >>= paintAll
    putStrLn $ printCanvas . canvas $ fs
