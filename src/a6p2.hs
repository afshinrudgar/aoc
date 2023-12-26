module Main where

import Data.Char (digitToInt, isDigit, isSpace)

main :: IO ()
main = do
  readFile "input/a6p2.txt" >>= print . solve

data Race = Race {time :: Integer, distance :: Integer} deriving (Show)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitByDelimiter :: (Eq a) => a -> [a] -> [[a]]
splitByDelimiter _ [] = []
splitByDelimiter sep s = h : (if null rst then [] else splitByDelimiter sep (tail rst))
  where
    (h, rst) = span (/= sep) s

parseNumsAsOne :: String -> Integer
parseNumsAsOne s = foldl (\acc curr -> acc * 10 + toInteger (digitToInt curr)) 0 (filter isDigit s)

parse :: String -> Race
parse s = Race time' distance'
  where
    ls = lines s
    timesStr = trim $ drop 1 $ dropWhile (/= ':') (head ls)
    distancesStr = trim $ drop 1 $ dropWhile (/= ':') (head (tail ls))
    time' = parseNumsAsOne timesStr
    distance' = parseNumsAsOne distancesStr

isWin :: Race -> Integer -> Bool
isWin r n = n * (time r - n) > distance r

-- timeToStart vs (time, distance)
-- n = timeToStart
-- time = Race.time
-- distance = Race.distance
-- n * (time - n) > distance
-- -n^2 + n*time - distance > 0 ~ ax^2 + bx + c > 0 AND a = -1 AND b = time AND c = -distance
-- x1 = -b +- sqrt(b^2 - 4ac) / 2a AND x2 = -b +- sqrt(b^2 - 4ac) / 2a
-- we need to count the positive numbers between min(x1, x2) and max(x1, x2)
countWinningWays :: Race -> Int
countWinningWays r = length $ filter (isWin r) [start .. end]
  where
    a = -1
    b = fromIntegral (time r)
    c = (-1) * fromIntegral (distance r)
    sq = sqrt (b * b - 4 * a * c)
    x1 = (-1 * b + sq) / (2 * a)
    x2 = (-1 * b - sq) / (2 * a)
    start = ceiling (min x1 x2)
    end = floor (max x1 x2)

solve :: String -> Int
solve s = countWinningWays (parse s)
