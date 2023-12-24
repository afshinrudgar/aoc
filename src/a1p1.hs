module Main (main) where

import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
  readLines "input/a1p1.txt" >>= print . solve

solve :: [String] -> Int
solve = sum . map toNumber

findFirst :: (a -> Bool) -> [a] -> a
findFirst predicate = head . filter predicate

findLast :: (a -> Bool) -> [a] -> a
findLast predicate = findFirst predicate . reverse

toNumber :: [Char] -> Int
-- toNumber s = ((* 10) . read . toList . findFirst isDigit s) + (read . toList . findLast isDigit s)
toNumber s = 10 * digitToInt (findFirst isDigit s) + digitToInt (findLast isDigit s)

-- Read a file line by line
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
