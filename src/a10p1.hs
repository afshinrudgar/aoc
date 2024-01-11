module Main where

import           Data.Foldable (find)
import           Data.Graph    (Graph, Tree (Node), Vertex, graphFromEdges, scc)
import           Data.Maybe    (fromJust, isJust)
import           Safe          (atMay)

main :: IO ()
main = do
  readFile "input/a10p1.txt" >>= print . solve

data Pipe = Vertical | Horizontal | NorthEastBound | NorthWestBound | SouthEastBound | SouthWestBound deriving (Eq)

data Tile = PipeTile Pipe | Ground | Start deriving (Eq)

parsePipe :: Char -> Pipe
parsePipe '|' = Vertical
parsePipe '-' = Horizontal
parsePipe 'L' = NorthEastBound
parsePipe 'J' = NorthWestBound
parsePipe '7' = SouthWestBound
parsePipe 'F' = SouthEastBound
parsePipe _   = error "Invalid pipe"

parseTile :: Char -> Tile
parseTile '.' = Ground
parseTile 'S' = Start
parseTile ch  = PipeTile $ parsePipe ch

parseLine :: String -> [Tile]
parseLine = map parseTile

parseGrid :: String -> [[Tile]]
parseGrid = map parseLine . lines

pipeConn :: Pipe -> (Int, Int) -> [(Int, Int)]
pipeConn Vertical (x, y)       = [(x - 1, y), (x + 1, y)]
pipeConn Horizontal (x, y)     = [(x, y - 1), (x, y + 1)]
pipeConn NorthEastBound (x, y) = [(x - 1, y), (x, y + 1)]
pipeConn NorthWestBound (x, y) = [(x - 1, y), (x, y - 1)]
pipeConn SouthEastBound (x, y) = [(x + 1, y), (x, y + 1)]
pipeConn SouthWestBound (x, y) = [(x + 1, y), (x, y - 1)]

isNorthBound :: Pipe -> Bool
isNorthBound NorthEastBound = True
isNorthBound NorthWestBound = True
isNorthBound Vertical       = True
isNorthBound _              = False

isEastBound :: Pipe -> Bool
isEastBound NorthEastBound = True
isEastBound SouthEastBound = True
isEastBound Horizontal     = True
isEastBound _              = False

isSouthBound :: Pipe -> Bool
isSouthBound SouthEastBound = True
isSouthBound SouthWestBound = True
isSouthBound Vertical       = True
isSouthBound _              = False

isWestBound :: Pipe -> Bool
isWestBound NorthWestBound = True
isWestBound SouthWestBound = True
isWestBound Horizontal     = True
isWestBound _              = False

check :: [[Tile]] -> (Pipe -> Bool) -> (Int, Int) -> Bool
check grid checkFunc (x, y) = isJust pipe && checkFunc (fromJust pipe)
  where
    tile = grid `atMay` x >>= (`atMay` y)
    pipe = case tile of
      Just (PipeTile p) -> Just p
      _                 -> Nothing

tileConn :: [[Tile]] -> Tile -> (Int, Int) -> [(Int, Int)]
tileConn _ (PipeTile pipe) pos = pipeConn pipe pos
tileConn _ Ground _ = []
tileConn grid Start (x, y) =
  (map fst . filter (\(pos, chk) -> check grid chk pos))
    [ ((x, y + 1), isWestBound),
      ((x, y - 1), isEastBound),
      ((x + 1, y), isNorthBound),
      ((x - 1, y), isSouthBound)
    ]

toGraph :: [[Tile]] -> (Graph, Vertex -> (Tile, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Vertex)
toGraph grid = (graphFromEdges . map (\(pos, tile) -> (tile, pos, tileConn grid tile pos)) . filter (\(_, tile) -> tile /= Ground)) (withPosition grid)

maxTreeDepth :: Tree Vertex -> Int
maxTreeDepth (Node _ [])       = 1
maxTreeDepth (Node _ children) = 1 + maximum (map maxTreeDepth children)

withPosition :: [[a]] -> [((Int, Int), a)]
withPosition grid = concatMap (\(x, row) -> zipWith (\y tile -> ((x, y), tile)) [0 ..] row) $ zip [0 ..] grid

findStart :: [[Tile]] -> (Int, Int)
findStart grid = fst $ fromJust $ find ((== Start) . snd) $ withPosition grid

inTree :: Vertex -> Tree Vertex -> Bool
inTree v (Node v' children) = v == v' || any (inTree v) children

maxDistanceFromStart :: Vertex -> Graph -> Int
maxDistanceFromStart s = flip div 2 . maximum . map maxTreeDepth . filter (inTree s) . scc

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

solve :: String -> Int
solve s = maxDistanceFromStart startVertex g
  where
    grid = parseGrid s
    (g, _, fromKey) = toGraph grid
    startVertex = (fromJust . fromKey . findStart) grid
