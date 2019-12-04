{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordPuns    #-}

module Day3
    ( day3
    ) where

import           Data.List       (sort, sortOn)
import qualified Data.Map.Strict as M
import           Paths_aoc19     (getDataFileName)
import           Text.Parsec     (Parsec, char, digit, many1, newline, parse,
                                  sepBy1, sepEndBy1, try, (<|>))

loadInput :: IO String
loadInput = getDataFileName "inputs/day-3.txt" >>= readFile

data Point =
    Point
        { x :: Integer
        , y :: Integer
        }
    deriving (Show, Eq, Ord)
    -- BAD Ord INSTANCE

distance :: Point -> Integer
distance Point {x, y} = abs x + abs y

data Direction
    = H
    | V
    deriving (Show, Eq, Ord)

data SegmentAbs =
    SegmentAbs
        { from      :: Point
        , to        :: Point
        , direction :: Direction
        }
    deriving (Show, Eq, Ord)

data Intersection =
    Intersection
        { point :: Point
        , segA  :: SegmentAbs
        , segB  :: SegmentAbs
        }
    deriving (Show, Eq)

intersect :: SegmentAbs -> SegmentAbs -> Maybe Intersection
intersect segA@SegmentAbs {direction = dirA} segB@SegmentAbs {direction = dirB}
    | dirA == dirB = Nothing
    | dirA == H = intersectHV segA segB
    | dirA == V = intersectHV segB segA
    -- REBINDING OF segA and segB
  where
    intersectHV SegmentAbs {to = toH, from = fromH} SegmentAbs { to = toV
                                                               , from = fromV
                                                               } =
        if between (x toH) (x fromH) (x toV) &&
           between (y toV) (y fromV) (y toH)
            then Just
                     Intersection
                         { point = Point {x = x toV, y = y toH}
                         , segA = segA
                         , segB = segB
                         }
            else Nothing
    between a b c
        -- INCLUSIVE
     =
        if a < b
            then (a < c) && (c < b)
            else (a > c) && (c > b)

data SegmentDelta
    = D Integer
    | U Integer
    | L Integer
    | R Integer
    deriving (Show)

segmentDirection ds =
    case ds of
        D _ -> V
        U _ -> V
        L _ -> H
        R _ -> H

newtype Wire a =
    Wire [a]
    deriving (Show, Functor)

deltaToAbs :: Point -> Wire SegmentDelta -> Wire SegmentAbs
deltaToAbs start (Wire deltaSegments) = Wire (go start deltaSegments [])
  where
    go _ [] result = result
    go start (ds:dss) result =
        let from = start
            to = move start ds
         in go to
                dss
                (result ++
                 [SegmentAbs {from, to, direction = segmentDirection ds}])
    move Point {x, y} ds =
        case ds of
            D down  -> Point {x, y = y - down}
            U up    -> Point {x, y = y + up}
            L left  -> Point {x = x - left, y}
            R right -> Point {x = x + right, y}

intersections :: Wire SegmentAbs -> Wire SegmentAbs -> [Intersection]
intersections (Wire absSegmentsA) (Wire absSegmentsB) =
    [ p
    | segA <- absSegmentsA
    , segB <- absSegmentsB
    -- pat :: t <- expr :: [t]
    , Just p <- [segA `intersect` segB]
    ]

wireLength :: Wire a -> Integer
wireLength (Wire segments) = toInteger (length segments)

parseWire :: Parsec String () (Wire SegmentDelta)
parseWire = Wire <$> parseSegmentDelta `sepBy1` char ','

parseSegmentDelta :: Parsec String () SegmentDelta
parseSegmentDelta = pD <|> pU <|> pL <|> pR
  where
    pD = try (D <$> (char 'D' *> number))
    pU = try (U <$> (char 'U' *> number))
    pL = try (L <$> (char 'L' *> number))
    pR = try (R <$> (char 'R' *> number))
    number = read <$> many1 digit

parseInput :: String -> [Wire SegmentDelta]
parseInput input =
    case parse (parseWire `sepEndBy1` newline) "" input of
        Left e   -> error $ show e
        Right ps -> ps

intersectionMap ::
       Wire SegmentAbs -> Wire SegmentAbs -> M.Map SegmentAbs [Point]
intersectionMap wireA wireB
    -- NO DUPLICATES IF THE LISTS ARE DIFFERENT
 =
    let intAB = intersections wireA wireB
        intBA = intersections wireB wireA
        map = M.empty
     in build (intAB ++ intBA) map
  where
    build [] map = map
    build (Intersection {point, segA, segB}:ints) map =
        build ints (M.insertWith (++) segA [point] map)

directedDistance :: Direction -> Point -> Point -> Integer
directedDistance direction from to
    | direction == H = abs (x from - x to)
    | direction == V = abs (y from - y to)

segmentLength :: SegmentAbs -> Integer
segmentLength SegmentAbs {from, to, direction} =
    directedDistance direction from to

intersectionDistances ::
       M.Map SegmentAbs [Point] -> Wire SegmentAbs -> M.Map Point [Integer]
intersectionDistances intMap (Wire segments) = go segments 0 M.empty
  where
    go [] _ map = map
    go (seg:segs) steps map =
        case M.findWithDefault [] seg intMap of
            [] -> go segs (steps + segmentLength seg) map
            ints ->
                let segFrom = from seg
                    sorted =
                        sortOn (directedDistance (direction seg) segFrom) ints
                 in goInts sorted segFrom steps map
                where segDir = direction seg
                      goInts [] from steps map =
                          go
                              segs
                              -- LAST PART OF SEGMENT
                              (steps + directedDistance segDir from (to seg))
                              map
                      goInts (int:ints) from steps map =
                          let newSteps =
                                  steps + directedDistance segDir from int
                           in goInts
                                  ints
                                  int
                                  newSteps
                                  (M.insert int [newSteps] map)

day3 :: IO ()
day3 = do
    (wireA:wireB:_) <- parseInput <$> loadInput
    let origin = Point 0 0
        absWireA = deltaToAbs origin wireA
        absWireB = deltaToAbs origin wireB
        intAB = intersections absWireA absWireB
    print $ minimum $ map (distance . point) intAB
    let intMap = intersectionMap absWireA absWireB
        distWireA = intersectionDistances intMap absWireA
        distWireB = intersectionDistances intMap absWireB
        pairs = M.elems $ M.unionWith (++) distWireA distWireB
    print $ minimum $ map sum pairs
