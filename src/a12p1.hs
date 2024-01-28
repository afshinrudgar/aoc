module Main where

main :: IO ()
main = do
  readFile "input/a12p1.txt" >>= print . solve

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs =
  let (h, t) = span (/= x) xs
   in h : splitOn x (drop 1 t)

readNums :: String -> [Int]
readNums = map read . splitOn ','

fit :: Int -> [Char] -> Bool
fit n s = length s >= n && notElem '.' (take n s) && (length s == n || s !! n /= '#')

numberOfArrangments' :: String -> [Int] -> Int
numberOfArrangments' s [] = if '#' `elem` s then 0 else 1
numberOfArrangments' [] _ = 0
numberOfArrangments' ('.' : ss) ns = numberOfArrangments' ss ns
numberOfArrangments' t (n : ns)
  | length (filter (/= '.') t) < sum (n : ns) = 0
  | head t == '?' && m = numberOfArrangments' (drop (n + 1) t) ns + numberOfArrangments' (drop 1 t) (n : ns)
  | head t == '#' && not m = 0
  | m = numberOfArrangments' (drop (n + 1) t) ns
  | otherwise = numberOfArrangments' (drop 1 t) (n : ns)
  where
    m = fit n t

numberOfArrangments :: String -> Int
numberOfArrangments s = numberOfArrangments' record nums
  where
    (record, nums') = span (/= ' ') s
    nums = readNums nums'

solve :: String -> Int
solve = sum . map numberOfArrangments . lines
