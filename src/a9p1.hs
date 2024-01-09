module Main where

import Data.Char (isDigit, isSpace)

main :: IO ()
main = do
  readFile "input/a9p1.txt" >>= print . solve

parseSeq :: String -> [Int]
parseSeq "" = []
parseSeq s = read h : parseSeq rst
  where
    (h, rst') = span (\ch -> isDigit ch || ch == '-') s
    rst = dropWhile isSpace rst'

parse :: String -> [[Int]]
parse = map parseSeq . lines

diff :: [Int] -> [Int]
diff [] = []
diff xs = zipWith (-) (tail xs) xs

extrapolate :: [Int] -> Int
extrapolate xs = if all (== 0) bottom then last xs else last xs + extrapolate bottom
  where
    bottom = diff xs

solve :: String -> Int
solve = sum . map extrapolate . parse
