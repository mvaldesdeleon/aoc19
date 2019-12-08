{-# LANGUAGE NamedFieldPuns #-}

module Day7
    ( day7
    ) where

import           Control.Monad
import           Data.Foldable       (traverse_)
import           Data.List           (permutations, reverse)
import           Data.Maybe          (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Vector.Mutable as V
import           Paths_aoc19         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-7.txt" >>= readFile

data ExecutionState
    = Ready
    | WaitingInput
    | Halted
    deriving (Show, Eq)

data ComputerState =
    ComputerState
        { memory         :: V.IOVector Integer
        , ip             :: Integer
        , input          :: [Integer]
        , output         :: [Integer]
        , executionState :: ExecutionState
        }

parseInput :: String -> IO ComputerState
parseInput string = do
    memory <- fromList (read $ "[" ++ string ++ "]")
    return $ ComputerState memory 0 [] [] Ready
  where
    fromList xs = do
        vector <- V.new (length xs)
        traverse_ (uncurry $ V.write vector) (zip [0 ..] xs)
        return vector

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
execute cs@ComputerState { memory
                         , ip
                         , input = is
                         , output = os
                         , executionState = es
                         } = do
    when (es == Halted) $ error "Attempting to execute from a Halted state"
    Instruction {opcode, p1Mode, p2Mode, p3Mode} <-
        parseInstruction <$> V.read memory (fromInteger ip)
    case opcode of
        1 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (firstV + secondV)
            return $ ComputerState memory (ip + 4) is os Ready
        2 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (firstV * secondV)
            return $ ComputerState memory (ip + 4) is os Ready
        3 -> do
            (first:_) <- readParameters cs 1
            case is of
                [] -> return $ ComputerState memory ip is os WaitingInput
                (i:is) -> do
                    V.write memory (fromInteger first) i
                    return $ ComputerState memory (ip + 2) is os Ready
        4 -> do
            (first:_) <- readParameters cs 1
            value <- V.read memory (fromInteger first)
            return $ ComputerState memory (ip + 2) is (value : os) Ready
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
                    is
                    os
                    Ready
        7 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (bit (firstV < secondV))
            return $ ComputerState memory (ip + 4) is os Ready
        8 -> do
            (first:second:third:_) <- readParameters cs 3
            firstV <- applyMode cs p1Mode first
            secondV <- applyMode cs p2Mode second
            V.write memory (fromInteger third) (bit (firstV == secondV))
            return $ ComputerState memory (ip + 4) is os Ready
        99 -> return $ ComputerState memory ip is os Halted
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

newtype Computer =
    Computer
        { runComputer :: [Integer] -> IO ([Integer], Maybe Computer)
        }

fromComputerState :: ComputerState -> Computer
fromComputerState cs =
    Computer $ \input -> do
        finalCs <- (setInput input <$> clone cs) >>= executeAll
        let result = output finalCs
        if executionState finalCs == Halted
            then return (result, Nothing)
            else return (result, Just $ fromComputerState finalCs {output = []})
  where
    setInput input cs = cs {input}

instance Semigroup Computer where
    ca <> cb =
        Computer $ \input -> do
            (resultA, newComputerA) <- runComputer ca input
            (resultB, newComputerB) <- runComputer cb (reverse resultA)
            when (isJust newComputerA && isNothing newComputerB) $
                error "Computer partially halted: rhs"
            when (isJust newComputerB && isNothing newComputerA) $
                error "Computer partially halted: lhs"
            return (resultB, sconcat $ catMaybes [newComputerA, newComputerB])

sconcat :: Semigroup a => [a] -> Maybe a
sconcat [] = Nothing
sconcat (a:as) =
    case sconcat as of
        Nothing -> Just a
        Just as -> Just $ a <> as

runAmps :: Computer -> [Integer] -> IO [Integer]
runAmps c ps = do
    primed <- forM ps (fmap (fromJust . snd) . runComputer c . pure)
    run (fromJust . sconcat $ primed) [0]
  where
    run c i = do
        (result, newComputer) <- runComputer c i
        case newComputer of
            Nothing -> return result
            Just nc -> run nc (reverse result)

day7 :: IO ()
day7 = do
    c <- fromComputerState <$> (loadInput >>= parseInput)
    outputs <- forM (permutations [0 .. 4]) (fmap head . runAmps c)
    print $ maximum outputs
    outputs <- forM (permutations [5 .. 9]) (fmap head . runAmps c)
    print $ maximum outputs
