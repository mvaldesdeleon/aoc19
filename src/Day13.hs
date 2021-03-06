{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Day13
    ( day13
    ) where

import           Control.Concurrent  (threadDelay)
import           Control.Monad
import           Data.Foldable       (traverse_)
import           Data.List           (permutations, reverse)
import           Data.Maybe          (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Vector.Mutable as V
import           Paths_aoc19         (getDataFileName)
import           Prelude             hiding (Left, Right)
import           System.Console.ANSI (clearScreen, hideCursor,
                                      setCursorPosition)
import           System.IO           (getChar, hReady, hSetEcho, stdin)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-13.txt" >>= readFile

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

clearOutput :: ComputerState -> ComputerState
clearOutput cs = cs {output = []}

data Tile
    = Empty
    | Wall
    | Block
    | Paddle
    | Ball
    deriving (Show, Eq)

parseTile :: Integer -> Tile
parseTile 0 = Empty
parseTile 1 = Wall
parseTile 2 = Block
parseTile 3 = Paddle
parseTile 4 = Ball

data Primitive
    = Sprite
          { position :: (Integer, Integer)
          , tile     :: Tile
          }
    | Score Integer
    deriving (Show, Eq)

parsePrimitives :: [Integer] -> [Primitive]
parsePrimitives [] = []
parsePrimitives (-1:0:score:rest) = Score score : parsePrimitives rest
parsePrimitives (x:y:t:rest) =
    Sprite {position = (x, y), tile = parseTile t} : parsePrimitives rest

printOutput :: ComputerState -> IO ()
printOutput ComputerState {output} = do
    let prims = parsePrimitives . reverse $ output
    forM_ prims $ \case
        Score score -> do
            setCursorPosition 0 0
            putStr . show $ score
        Sprite {..} -> do
            setCursorPosition
                ((+ 1) . fromInteger . snd $ position)
                (fromInteger . fst $ position)
            putChar . printTile $ tile
  where
    printTile Empty  = ' '
    printTile Wall   = '█'
    printTile Block  = '░'
    printTile Paddle = '═'
    printTile Ball   = '°'

data AIState =
    AIState
        { ball   :: (Integer, Integer)
        , paddle :: Integer
        }
    deriving (Show)

updateAIState :: ComputerState -> AIState -> AIState
updateAIState ComputerState {output} AIState {..} =
    let prims = parsePrimitives . reverse $ output
        balls = [p | Sprite p Ball <- prims]
        paddles = [fst p | Sprite p Paddle <- prims]
     in AIState
            { ball = headWithDefault ball balls
            , paddle = headWithDefault paddle paddles
            }
  where
    headWithDefault d []    = d
    headWithDefault d (a:_) = a

mainLoop :: ComputerState -> AIState -> IO ()
mainLoop cs ais = do
    ns <- executeAll cs
    let nais = updateAIState ns ais
    printOutput ns
    if executionState ns == Halted
        then do
            setCursorPosition 8 14
            putStr "GAME OVER"
            setCursorPosition 25 0
        else do
            rdy <- hReady stdin
            input <-
                if rdy
                    then parseInput <$> getChar
                    else return $ assist nais
            threadDelay 20000
            mainLoop (setInput [input] . clearOutput $ ns) nais
  where
    parseInput 'a' = -1
    parseInput 'd' = 1
    parseInput _   = 0

assist :: AIState -> Integer
assist (AIState (ball, _) paddle) =
    let delta = ball - paddle
     in if abs delta >= 1
            then signum delta
            else 0

day13 :: IO ()
day13 = do
    cs <- loadInput >>= parseInput
    -- fs <- executeAll cs
    -- let sprites = parseSprites . reverse . output $ fs
    -- print $ length . filter ((== Block) . tile) $ sprites
    clearScreen
    hideCursor
    hSetEcho stdin False
    mainLoop (setInput [0] cs) (AIState (0, 0) 0)
