module Day10
    ( day10
    ) where

import           Data.Function (on)
import           Data.List     (groupBy, maximumBy, sort)
import           Data.Ratio
import qualified Data.Set      as S
import           Paths_aoc19   (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-10.txt" >>= readFile

newtype Asteroid =
    Asteroid (Integer, Integer)
    deriving (Show, Eq, Ord)

x :: Asteroid -> Integer
x (Asteroid (val, _)) = val

y :: Asteroid -> Integer
y (Asteroid (_, val)) = val

newtype Delta =
    Delta (Integer, Integer)
    deriving (Show, Eq)

data SwipeAngle
    = VerticalUp
    | RightSide Rational
    | VerticalDown
    | LeftSide Rational
    deriving (Show, Eq, Ord)

swipeAngle :: Delta -> SwipeAngle
swipeAngle (Delta (dx, dy)) =
    case signum dx of
        0 ->
            case signum dy of
                0  -> error "Can't compute SwipeAngle from Delta (0, 0)"
                1  -> VerticalDown
                -1 -> VerticalUp
        1 -> RightSide (dy % dx)
        -1 -> LeftSide (dy % dx)

sqlen :: Delta -> Integer
sqlen (Delta (dx, dy)) = dx * dx + dy * dy

instance Ord Delta where
    (Delta (0, 0)) <= _ = True
    _ <= (Delta (0, 0)) = False
    da <= db =
        let saa = swipeAngle da
            sab = swipeAngle db
         in if saa == sab
                then sqlen da <= sqlen db
                else saa <= sab

opposite :: Delta -> Delta
opposite (Delta (dx, dy)) = Delta (-dx, -dy)

parseInput :: String -> [Asteroid]
parseInput = concatMap (uncurry parseRow) . withIndex 0 . lines
  where
    withIndex i = zip [i ..]

parseRow :: Integer -> String -> [Asteroid]
parseRow y row = go 0 row []
  where
    go _ [] result       = result
    go x ('#':as) result = go (x + 1) as (Asteroid (x, y) : result)
    go x (_:as) result   = go (x + 1) as result

deltaAsteroid :: Delta -> Asteroid -> Asteroid
deltaAsteroid (Delta (dx, dy)) (Asteroid (x, y)) = Asteroid (x + dx, y + dy)

outOfBounds :: Integer -> Integer -> Asteroid -> Bool
outOfBounds width height (Asteroid (x, y)) =
    x < 0 || y < 0 || x > width || y > height

countVisible :: Integer -> Integer -> S.Set Asteroid -> Asteroid -> Integer
countVisible width height asteroids a = go (S.delete a asteroids) 0
  where
    go asteroids result
        | S.null asteroids = result
        | otherwise =
            let b = S.findMin asteroids
                delta = ratio a b
                (countA, restA) = explore delta asteroids
                (countB, restB) = explore (opposite delta) restA
             in go restB (result + countA + countB)
    explore :: Delta -> S.Set Asteroid -> (Integer, S.Set Asteroid)
    explore delta asteroids = go (deltaAsteroid delta a) asteroids 0
      where
        go b asteroids result
            | outOfBounds width height b = (result, asteroids)
            | otherwise =
                let nextB = deltaAsteroid delta b
                 in if S.member b asteroids
                        then go nextB (S.delete b asteroids) 1
                        else go nextB asteroids result

ratio :: Asteroid -> Asteroid -> Delta
ratio (Asteroid (xa, ya)) (Asteroid (xb, yb)) =
    let dx = xa - xb
        dy = ya - yb
        factor = dx `gcd` dy
     in Delta (dx `div` factor, dy `div` factor)

relativeTo :: Asteroid -> Asteroid -> Delta
relativeTo (Asteroid (xa, ya)) (Asteroid (xb, yb)) = Delta (xa - xb, ya - yb)

translateBy :: Asteroid -> Delta -> Asteroid
translateBy (Asteroid (x, y)) (Delta (dx, dy)) = Asteroid (x + dx, y + dy)

asteroidsDetected :: [Asteroid] -> [(Integer, Asteroid)]
asteroidsDetected as =
    let width = maximum . map x $ as
        height = maximum . map y $ as
        asteroids = S.fromList as
     in map (addCount width height asteroids) as
  where
    addCount width height as a = (countVisible width height as a, a)

laserSwipe :: Asteroid -> [Asteroid] -> [Asteroid]
laserSwipe a =
    map (a `translateBy`) .
    consume .
    groupBy ((==) `on` swipeAngle) . sort . map (`relativeTo` a) . filter (/= a)
  where
    consume dss = go dss []
      where
        go [] results = results
        go dss results =
            let (heads, tails) = swipe dss
             in go tails (results ++ heads)
    swipe = foldr headsAndTails ([], [])
      where
        headsAndTails [] (hs, ts)     = (hs, ts)
        headsAndTails [a] (hs, ts)    = (a : hs, ts)
        headsAndTails (a:as) (hs, ts) = (a : hs, as : ts)

day10 :: IO ()
day10 = do
    as <- parseInput <$> loadInput
    let (count, a) = maximumBy (compare `on` fst) . asteroidsDetected $ as
    print count
    let (Asteroid (x, y)) = laserSwipe a as !! 199
    print $ 100 * x + y
