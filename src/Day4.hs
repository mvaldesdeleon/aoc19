module Day4
    ( day4
    ) where

from = 382345

to = 843167

passwords =
    [ p
    | a <- [3 .. 8]
    , b <- [0 .. 9]
    , c <- [0 .. 9]
    , d <- [0 .. 9]
    , e <- [0 .. 9]
    , f <- [0 .. 9]
    , let p = a * 100000 + b * 10000 + c * 1000 + d * 100 + e * 10 + f
    , p >= from
    , p <= to
    , a == b || b == c || c == d || d == e || e == f
    , a <= b
    , b <= c
    , c <= d
    , d <= e
    , e <= f
    ]

betterPasswords =
    [ p
    | a <- [3 .. 8]
    , b <- [0 .. 9]
    , c <- [0 .. 9]
    , d <- [0 .. 9]
    , e <- [0 .. 9]
    , f <- [0 .. 9]
    , let p = a * 100000 + b * 10000 + c * 1000 + d * 100 + e * 10 + f
    , p >= from
    , p <= to
    , (a == b && b /= c) ||
          (a /= b && b == c && c /= d) ||
          (b /= c && c == d && d /= e) ||
          (c /= d && d == e && e /= f) || (d /= e && e == f)
    , a <= b
    , b <= c
    , c <= d
    , d <= e
    , e <= f
    ]

day4 :: IO ()
day4 = do
    print $ length passwords
    print $ length betterPasswords
