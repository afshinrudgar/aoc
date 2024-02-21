module Main where
import Data.Char (ord)

main :: IO ()
main = do
  readFile "input/a15p1.txt" >>= print . solve

hash :: String -> Int
hash = foldl (\curr c -> (curr + ord c) * 17 `rem` 256) 0

splitByComma :: String -> [String]
splitByComma [] = []
splitByComma (',':xs) = splitByComma xs
splitByComma xs = h:splitByComma t
    where
        (h, t) = span (/= ',') xs

solve :: String -> Int
solve = sum . map hash . splitByComma . filter (/= '\n')
