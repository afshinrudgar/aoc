module Main where

import           Data.Char (isDigit, isSpace)

main :: IO ()
main = do
  readFile "input/a9p2.txt" >>= print . solve

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

extrapolatePrevious :: [Int] -> Int
extrapolatePrevious xs = if all (== 0) bottom then head xs else head xs - extrapolatePrevious bottom
  where
    bottom = diff xs

solve :: String -> Int
solve = sum . map extrapolatePrevious . parse
