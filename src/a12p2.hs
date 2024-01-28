{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

main :: IO ()
main = do
  readFile "input/a12p2.txt" >>= print . solve

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs =
  let (h, t) = span (/= x) xs
   in h : splitOn x (drop 1 t)

readNums :: String -> [Int]
readNums = map read . splitOn ','

fit :: [Char] -> Int -> Bool
fit s n = length s >= n && notElem '.' (take n s) && (length s == n || s !! n /= '#')

numberOfArrangments' :: (String, [Int]) -> ((Int, Int) -> Int) -> (Int, Int) -> Int
numberOfArrangments' (s, ns) fm (i, j) = res
  where
    t = drop i s
    m = (j < length ns) && fit t (ns!!j)
    res
      | j >= length ns = if '#' `elem` drop i s then 0 else 1
      | i >= length s  = 0
      | length (filter (/= '.') t) < sum (drop j ns) = 0
      | head t == '.' = fm (i+1, j)
      | head t == '?' && m = fm (i+ns!!j+1, j+1) + fm (i+1, j)
      | head t == '#' && not m = 0
      | m = fm (i+ns!!j+1, j+1)
      | otherwise = fm (i+1, j)

fList :: (String, [Int]) -> [[Int]]
fList (s, ns) = [
    [numberOfArrangments' (s,ns) (f_fast (s, ns)) (i, j) | j <- [0..length ns+1]]
    | i <- [0..length s+1]
  ]

f_fast' :: [[Int]] -> (Int, Int) -> Int
f_fast' tree (i, j) = if i >= length tree || j >= length (head tree) then 0 else tree !! i !! j

f_fast :: (String, [Int]) -> (Int, Int) -> Int
f_fast (s, ns) = f_fast' (fList (s, ns))

numberOfArrangments :: String -> Int
numberOfArrangments ss = f_fast (s, ns) (0, 0)
  where
    (record, nums') = span (/= ' ') ss
    nums = readNums nums'
    s = foldl1 (\acc curr -> acc ++ "?" ++ curr) (replicate 5 record)
    ns = concat (replicate 5 nums)

solve :: String -> Int
solve = sum . map numberOfArrangments . lines
