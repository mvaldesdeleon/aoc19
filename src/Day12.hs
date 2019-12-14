{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Day12
    ( day12
    ) where

import           Data.Char     (isDigit)
import qualified Data.Map.Lazy as M
import           Paths_aoc19   (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-12.txt" >>= readFile

newtype V3 =
    V3 (Integer, Integer, Integer)
    deriving (Show, Eq, Ord)

neg :: V3 -> V3
neg (V3 (vx, vy, vz)) = V3 (-vx, -vy, -vz)

add :: V3 -> V3 -> V3
add (V3 (xa, ya, za)) (V3 (xb, yb, zb)) = V3 (xa + xb, ya + yb, za + zb)

x :: V3 -> Integer
x (V3 (v, _, _)) = v

y :: V3 -> Integer
y (V3 (_, v, _)) = v

z :: V3 -> Integer
z (V3 (_, _, v)) = v

absSum :: V3 -> Integer
absSum (V3 (vx, vy, vz)) = abs vx + abs vy + abs vz

data Moon =
    Moon
        { position :: V3
        , velocity :: V3
        }
    deriving (Show, Eq, Ord)

gravity :: Moon -> Moon -> V3
gravity Moon {position = pa} Moon {position = pb} =
    let gx = signum (x pb - x pa)
        gy = signum (y pb - y pa)
        gz = signum (z pb - z pa)
     in V3 (gx, gy, gz)

parseMoons :: String -> [Moon]
parseMoons = map parseMoon . lines
  where
    parseMoon s =
        let [x, y, z] = read . wrap . filter keep $ s
         in Moon {position = V3 (x, y, z), velocity = V3 (0, 0, 0)}
    keep c = isDigit c || c == ',' || c == '-'
    wrap s = "[" ++ s ++ "]"

simulationStep :: [Moon] -> [Moon]
simulationStep = map applyVelocity . applyGravity
  where
    applyGravity ms = map (applyGravitySingle ms) ms
      where
        applyGravitySingle ms m =
            m {velocity = foldr (add . (m `gravity`)) (velocity m) ms}
    applyVelocity Moon {..} =
        Moon {position = position `add` velocity, velocity}

energy :: Moon -> Integer
energy Moon {..} = absSum position * absSum velocity

data Moon1D =
    Moon1D
        { position1D :: Integer
        , velocity1D :: Integer
        }
    deriving (Show, Eq, Ord)

moonX :: Moon -> Moon1D
moonX Moon {..} = Moon1D {position1D = x position, velocity1D = x velocity}

moonY :: Moon -> Moon1D
moonY Moon {..} = Moon1D {position1D = y position, velocity1D = y velocity}

moonZ :: Moon -> Moon1D
moonZ Moon {..} = Moon1D {position1D = z position, velocity1D = z velocity}

gravity1D :: Moon1D -> Moon1D -> Integer
gravity1D Moon1D {position1D = pa} Moon1D {position1D = pb} = signum (pb - pa)

simulationStep1D :: [Moon1D] -> [Moon1D]
simulationStep1D = map applyVelocity1D . applyGravity1D
  where
    applyGravity1D ms = map (applyGravity1DSingle ms) ms
      where
        applyGravity1DSingle ms m =
            m {velocity1D = foldr ((+) . (m `gravity1D`)) (velocity1D m) ms}
    applyVelocity1D Moon1D {..} =
        Moon1D {position1D = position1D + velocity1D, velocity1D}

findCycle :: Ord a => [a] -> (Integer, Integer)
findCycle vs = go vs 0 M.empty
  where
    go :: Ord a => [a] -> Integer -> M.Map a Integer -> (Integer, Integer)
    go (v:vs) i vals =
        case v `M.lookup` vals of
            Just iv -> (iv, i - iv)
            Nothing -> go vs (i + 1) (M.insert v i vals)

day12 :: IO ()
day12 = do
    ms <- parseMoons <$> loadInput
    let simulation = iterate simulationStep ms
    print $ sum . map energy . (!! 1000) $ simulation
    let (_, periodX) = findCycle $ iterate simulationStep1D (map moonX ms)
        (_, periodY) = findCycle $ iterate simulationStep1D (map moonY ms)
        (_, periodZ) = findCycle $ iterate simulationStep1D (map moonZ ms)
    print $ periodX `lcm` periodY `lcm` periodZ
