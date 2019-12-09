module Day8
    ( day8
    ) where

import           Control.Applicative (liftA2)
import           Data.Function       (on)
import           Data.List           (genericSplitAt, intercalate, minimumBy)
import           Paths_aoc19         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-8.txt" >>= readFile

width :: Integer
width = 25

height :: Integer
height = 6

chunks :: Integer -> [a] -> [[a]]
chunks _ [] = []
chunks size as =
    let (chunk, rest) = genericSplitAt size as
     in chunk : chunks size rest

data Pixel
    = Black
    | White
    | Transparent
    deriving (Show, Eq)

instance Semigroup Pixel where
    Transparent <> b = b
    a <> _ = a

instance Monoid Pixel where
    mempty = Transparent

parsePixel :: Char -> Pixel
parsePixel c =
    case c of
        '0' -> Black
        '1' -> White
        '2' -> Transparent
        _   -> error $ "Invalid pixel " ++ [c]

renderPixel :: Pixel -> Char
renderPixel p =
    case p of
        Black       -> ' '
        White       -> '#'
        Transparent -> '_'

newtype Layer =
    Layer [Pixel]
    deriving (Show)

instance Semigroup Layer where
    Layer pas <> Layer pbs = Layer $ zipWith (<>) pas pbs

instance Monoid Layer where
    mempty = Layer $ repeat Transparent

parseLayers :: String -> [Layer]
parseLayers =
    map (Layer . map parsePixel) . chunks (width * height) . filter (/= '\n')

renderLayer :: Layer -> String
renderLayer (Layer ps) = intercalate "\n" . chunks width $ map renderPixel ps

countPixels :: Layer -> (Integer, Integer, Integer)
countPixels = go (0, 0, 0)
  where
    go count (Layer []) = count
    go (c0, c1, c2) (Layer (p:ps)) =
        case p of
            Black       -> go (c0 + 1, c1, c2) (Layer ps)
            White       -> go (c0, c1 + 1, c2) (Layer ps)
            Transparent -> go (c0, c1, c2 + 1) (Layer ps)

zeros :: (Integer, Integer, Integer) -> Integer
zeros (c, _, _) = c

ones :: (Integer, Integer, Integer) -> Integer
ones (_, c, _) = c

twos :: (Integer, Integer, Integer) -> Integer
twos (_, _, c) = c

day8 :: IO ()
day8 = do
    layers <- parseLayers <$> loadInput
    print $
        liftA2 (*) ones twos $
        minimumBy (compare `on` zeros) $ countPixels <$> layers
    putStrLn $ renderLayer $ mconcat layers
