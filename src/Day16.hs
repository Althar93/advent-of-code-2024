module Day16 (day16Solver) where

import Parser
import Data.List
import Data.Maybe

import System.Console.ANSI
import System.Sleep

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day16_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day16_input.txt"

-- A tile type
data Tile = Start | End | Empty | Wall deriving (Show, Eq)

-- A move
--data Move = Move (Int, Int) | Turn (Int, Int) deriving (Show, Eq)

-- A node
data Node = Node {
    pos  :: !(Int, Int),
    dir  :: !(Int, Int),
    from :: !(Maybe Node),
    cost :: !Int
} deriving (Eq, Show)

-- Unfolds a node into it's full chain from start to finish
unfoldNode :: Node -> [Node]
unfoldNode n = case from n of
    Nothing -> [n]
    Just n' -> (unfoldNode n') ++ [n]

unfoldNodeMaybe :: Maybe Node -> [Node]
unfoldNodeMaybe Nothing  = []
unfoldNodeMaybe (Just n) = unfoldNode n

-- Makes a new node
mkNode :: (Int, Int) -> (Int, Int) -> Maybe Node -> Int -> Node
mkNode  p d n c = Node { pos = p, dir = d, from = n, cost = c }

-- An association of neighbours for a given position & direction
type NeighbourAssoc = ((Int, Int), (Int, Int)) -> [((Int, Int),(Int, Int))]

-- An association of cost for two consecutive positions & directions
type CostAssoc = ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Int

-- A star algorithm
astar :: NeighbourAssoc -> CostAssoc -> (Int, Int) -> [Node] -> [Node] -> Maybe Node
astar fp fc e os cs     | null os      = Nothing
                        | pos p == e   = Just p
                        | otherwise    = astar fp fc e os' cs' 
                        where
                            p   = head $ sortBy (\a b -> (cost a) `compare` (cost b)) os
                            ps  = fp (pos p, dir p)
                            cs' = (p:cs)
                            os' = foldl queue (delete p os) ps
                            queue a (xy,uv) = case find (\n -> (pos n) == xy && (dir n) == uv) (os ++ cs) of
                                                Nothing -> (mkNode xy uv (Just p) ((fc (xy, uv) (pos p, dir p)) + (cost p))) : a
                                                Just _  -> a

astar' :: NeighbourAssoc -> CostAssoc -> (Int, Int) -> [Node] -> [Node] -> [Node]
astar' fp fc e os cs    | null os      = []
                        | otherwise    = case filter (\p -> pos p == e) pb of
                            (x:xs) -> (x:xs)
                            []     -> concat [astar' fp fc e os' cs' | p <- pb, let ps = fp (pos p, dir p), let cs' = (p:cs), let os' = foldl (queue p) (delete p os) ps]
                            where
                                pb  = filter (\a -> (cost a) == cost (head ps)) ps
                                ps  = sortBy (\a b -> (cost a) `compare` (cost b)) os
                                queue p a (xy,uv) = case find (\n -> (pos n) == xy && (dir n) == uv) (os ++ cs) of
                                                        Nothing -> (mkNode xy uv (Just p) ((fc (xy, uv) (pos p, dir p)) + (cost p))) : a
                                                        Just _  -> a

-- A maze
type Maze = [[Tile]]

-- Parses the map part
parseMap :: Parser [[Char]]
parseMap = some parseMapLine
    where
        parseMapLine = do
            _ <- parseSpaces
            some $ parseIsOneOf "#.ES"

-- Converts a char to a tile type
charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '#' = Wall
charToTile 'S' = Start
charToTile 'E' = End
charToTile  _  = error "Char not recognised"
        
-- Makes a warehouse from the given unprocessed map & set of moves
mkMaze :: [[Char]] -> Maze
mkMaze xss = map (map charToTile) xss

-- Parses a maze
parseMaze :: Parser Maze
parseMaze = do 
    xss <- some parseMapLine
    return $ mkMaze xss
    where
        parseMapLine = do
            _ <- parseSpaces
            some $ parseIsOneOf "#.ES"

-- Reads the test input
readInputs :: IO Maze
readInputs = do
    contents <- readFile testInputFile
    pure $ runParser parseMaze contents

-- Draws a maze
drawMaze :: Maze -> [Node] -> IO ()
drawMaze xss ms = mapM_ drawRow [0..h-1] 
    where
    w                   = length (head xss)
    h                   = length xss
    drawRow y           = putStrLn $ map (coordToSymbol y) [0..w-1] 
    coordToSymbol y x   = case find (\n -> pos n == (x, y)) ms of
        Just n  -> case dir n of
                    ( 1,  0) -> '>'
                    (-1,  0) -> '<'
                    ( 0,  1) -> 'v'
                    ( 0, -1) -> '^'
        Nothing -> case (xss !! y !! x) of
                    Empty -> '.'
                    Wall  -> '#'
                    Start -> 'S'
                    End   -> 'E'

position :: Maze -> Tile -> (Int, Int)
position xss t = head [(x, y) | x <- [0..w - 1], y <- [0..h - 1], ((xss !! y) !! x) == t]
    where
        w = length (head xss)
        h = length xss

neighbours :: Maze -> ((Int, Int), (Int, Int)) -> [((Int, Int), (Int, Int)) ]
neighbours xss ((x0, y0), (u0, v0)) = forward ++ left ++ right
    where
        forward = [((x, y), (u0,    v0)) | let x = x0 + u0, let y = y0 + v0, ((xss !! y) !! x) /= Wall] 
        left    = [((x, y), (v0,   -u0)) | let x = x0 + v0, let y = y0 - u0, ((xss !! y) !! x) /= Wall] 
        right   = [((x, y), (-v0,   u0)) | let x = x0 - v0, let y = y0 + u0, ((xss !! y) !! x) /= Wall] 

riskCost :: Maze -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Int
riskCost xss (_, uv0) (_, uv1)  | uv0 /= uv1 = 1001
                                | otherwise  = 1

-- The solver for part #1 of the puzzle
solvePart1 :: Maze -> Int
solvePart1 xss = cost $ fromJust n
    where
        ps = map pos ns
        ns = unfoldNodeMaybe n
        n  = astar (neighbours xss) (riskCost xss) ep [s] []
        s  = mkNode sp (1, 0) Nothing 0
        sp = position xss Start
        ep = position xss End

-- The solver for part #2 of the puzzle
solvePart2 :: Maze -> Int
solvePart2 xss = length ps'--cost $ fromJust n
    where
        ps' = nub ps
        ps  = map pos ns'
        ns' = concat $ map unfoldNode ns
        ns  = astar' (neighbours xss) (riskCost xss) ep [s] []
        s   = mkNode sp (1, 0) Nothing 0
        sp  = position xss Start
        ep  = position xss End

-- The full solver
day16Solver :: IO [Int]
day16Solver = do
    input <- readInputs
    --drawMaze input []

    --let sp = position input Start
    --let ep = position input End
    --let s  = mkNode sp (1, 0) Nothing 0
    --let n  = astar' (neighbours input) (riskCost input) ep [s] []
    --putStrLn $ show n
    --let n' = unfoldNodeMaybe n
    --let p  = map pos n'

    --putStrLn $ show n

    --drawMaze input n'

    --let s = mkNode (1, 13) (0, -1) Nothing 0
    --let e = (13, 1)
    --let n = astar (neighbours input) (riskCost input) e [s] []
    --let p = unfoldNodeMaybe n
    --putStrLn $ show p

    return [solvePart1 input, solvePart2 input]
