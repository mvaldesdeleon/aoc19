module Day6
    ( day6
    ) where

import           Data.Foldable   (asum)
import qualified Data.Map.Strict as M
import           Paths_aoc19     (getDataFileName)
import           Text.Parsec     (Parsec, char, count, newline, oneOf, parse,
                                  sepEndBy1)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-6.txt" >>= readFile

newtype Object =
    Object String
    deriving (Show, Eq, Ord)

parseObject :: Parsec String () Object
parseObject = Object <$> count 3 (oneOf $ ['A' .. 'Z'] ++ ['0' .. '9'])

newtype Orbit =
    Orbit (Object, Object)

parseOrbits :: Parsec String () [Orbit]
parseOrbits =
    (mkOrbit <$> parseObject <* char ')' <*> parseObject) `sepEndBy1` newline
  where
    mkOrbit pa pb = Orbit (pa, pb)

loadOrbits :: IO [Orbit]
loadOrbits = do
    Right orbits <- parse parseOrbits "" <$> loadInput
    return orbits

data Tree a =
    Tree a [Tree a]
    deriving (Show)

planetTree :: [Orbit] -> Tree Object
planetTree orbits =
    buildTree (M.fromListWith (++) . map keyValue $ orbits) (Object "COM")
  where
    keyValue (Orbit (pa, pb)) = (pa, [pb])
    buildTree orbitMap planet =
        Tree
            planet
            (buildTree orbitMap <$> M.findWithDefault [] planet orbitMap)

countOrbits :: Tree a -> Integer
countOrbits = sumBranches 0
  where
    sumBranches base (Tree _ tas) = base + sum (sumBranches (base + 1) <$> tas)

findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath v (Tree a tas)
    | a == v = Just [a]
    | otherwise = (a :) <$> asum (findPath v <$> tas)

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (a:as) (b:bs) =
    if a == b
        then a : commonPrefix as bs
        else []

day6 :: IO ()
day6 = do
    tree <- planetTree <$> loadOrbits
    print $ countOrbits tree
    let Just you = findPath (Object "YOU") tree
        Just santa = findPath (Object "SAN") tree
    print $ length you + length santa - 2 * length (commonPrefix you santa) - 2
