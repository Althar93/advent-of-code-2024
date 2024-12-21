module Day15 (day15Solver) where

import Parser
import Data.List

import System.Console.ANSI
import System.Sleep

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day15_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day15_input.txt"

-- A tile type
data Tile = Robot | Wall | Box | BoxL | BoxR deriving (Show, Eq)

-- A warehouse
data Warehouse = Warehouse { 
    width   :: Int,
    height  :: Int,
    tiles   :: [(Int, Int, Tile)]
} deriving (Show)

-- A singular move
type Move = (Int, Int)

-- Parses the move part
parseMoves :: Parser [Move]
parseMoves = some parseMove
    where
        parseMove = do
            _ <- parseSpaces
            c <- parseIsOneOf "<>^v"
            case c of
              '<' -> return (-1,  0)
              '>' -> return ( 1,  0)
              '^' -> return ( 0, -1)
              'v' -> return ( 0,  1)

-- Parses the map part
parseMap :: Parser [[Char]]
parseMap = some parseMapLine
    where
        parseMapLine = do
            _ <- parseSpaces
            some $ parseIsOneOf "#.O@"

-- Converts a char to a tile type
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile 'O' = Box
charToTile '@' = Robot
charToTile  _  = error "Char not recognised"
        
-- Makes a warehouse from the given unprocessed map & set of moves
mkWarehouse :: [[Char]] ->Warehouse
mkWarehouse xss = Warehouse { width = w, height = h, tiles = ts }
    where
        w     = length (head xss)
        h     = length xss
        ts    = [(x, y, charToTile c) | x <- [0..w - 1], y <- [0..h - 1], let c = ((xss !! y) !! x), c `elem` "#O@"]

-- Expands a warehouse
expandWarehouse :: Warehouse -> Warehouse
expandWarehouse w = w { tiles = ts', width = 2 * (width w) }
    where
        ts  = tiles w
        ts' = expandTiles ts
        -- Tile expansion
        expandTiles [] = []
        expandTiles (t:ts) = case t of
            (x, y, Wall)  -> (2 * x, y, Wall)  : (2 * x + 1, y, Wall) : expandTiles ts
            (x, y, Box )  -> (2 * x, y, BoxL)  : (2 * x + 1, y, BoxR) : expandTiles ts
            (x, y, Robot) -> (2 * x, y, Robot) : expandTiles ts

-- Parses a warehouse and moves
parseWarehouseAndMoves :: Parser (Warehouse, [Move])
parseWarehouseAndMoves = do
    xss <- parseMap
    _ <- parseSpaces
    yss <- parseMoves
    return $ (mkWarehouse xss, yss)

-- Reads the test input
readInputs :: IO (Warehouse, [Move])
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseWarehouseAndMoves contents

-- Returns the robot's position
robotPosition :: Warehouse -> (Int, Int)
robotPosition w = case find (\(_, _, t) -> t == Robot) (tiles w) of
    Nothing         -> error "No robot found"
    Just (x, y, _)  -> (x, y)

-- Returns the box positions
boxPositions :: Warehouse -> [(Int, Int)]
boxPositions w = case filter (\(_, _, t) -> t == Box || t == BoxL) (tiles w) of 
    [] -> error "No boxes found"
    xs -> map (\(x, y, _) -> (x, y)) xs

-- Chains possible moves
chainMove :: [(Int, Int, Tile)] -> Move -> [(Int, Int)] -> [(Int, Int)] 
chainMove _   _     []              = error "Need at least one initial position"
chainMove ts (u, v) ((x0, y0):xys)  = case find (\(x, y, _) -> (x, y) == (x0 + u, y0 + v)) ts of
                                            Nothing             -> (x0, y0):xys                         -- No obstacles, our chain is complete
                                            Just (xO, yO, tO)   -> case tO of                           -- Obstacle, chain
                                                Wall  -> []                                             -- Wall encountered, stop
                                                Box   -> chainMove ts (u, v) ((xO, yO):(x0, y0):xys)    -- Chain move
                                                Robot -> []
                                                -- Special cases for part 2
                                                BoxL  -> if (abs u) > 0 
                                                    then chainMove ts (u, v) ((xO, yO):(x0, y0):xys)    -- Lateral movement, same as before
                                                    else case (chainMove ts (u, v) ((xO, yO):(x0, y0):xys), chainMove ts (u, v) [(xO + 1, yO)]) of
                                                        ([], _)       -> []
                                                        (_, [])       -> []
                                                        (left, right) -> left ++ right
                                                BoxR  -> if (abs u) > 0 
                                                    then chainMove ts (u, v) ((xO, yO):(x0, y0):xys)    -- Lateral movement, same as before
                                                    else case (chainMove ts (u, v) ((xO, yO):(x0, y0):xys), chainMove ts (u, v) [(xO - 1, yO)]) of
                                                        ([], _)       -> []
                                                        (_, [])       -> []
                                                        (left, right) -> left ++ right

-- Applies moves
applyMoves :: Move -> [(Int, Int)] -> [(Int, Int, Tile)] -> [(Int, Int, Tile)]
applyMoves _      [] ts             = ts -- Applied all moves
applyMoves _      xs []             = [] -- No more tiles to move
applyMoves (u, v) xs ((x, y, t):ts) = case findIndex (==(x, y)) xs of
                                            Nothing -> (x,     y,     t) : applyMoves (u, v) xs ts                 -- Nothing to move, keep going
                                            Just k  -> (x + u, y + v, t) : applyMoves (u, v) (deleteIndex k xs) ts -- Apply & consume move

-- Deletes an element from a list at the specified index
deleteIndex :: Int -> [a] -> [a]
deleteIndex _ []     = []
deleteIndex 0 (x:xs) = xs
deleteIndex n (x:xs) = x : deleteIndex (n - 1) xs

-- Steps the warehouse once
stepOne :: (Int, Int) -> Move -> Warehouse -> Warehouse
stepOne (x, y) (u, v) w = case null ms of
    True  -> w
    False -> w { tiles = t' }
    where
        (x', y')       = (x + u, y + v)
        t'             = applyMoves (u, v) ms t
        ms             = chainMove t (u, v) [(x, y)]
        t              = tiles w

-- Steps the warehouse through all moves
stepMany :: [Move] -> Warehouse -> Warehouse
stepMany [] w = w
stepMany ms w = case ms of
    []        -> w 
    otherwise -> stepMany (tail ms) w'
    where
        w' = stepOne (robotPosition w) (head ms) w

-- Returns the GPS coordinates of a map position
gpsCoordinates :: (Int, Int) -> Int
gpsCoordinates (x, y) = x + y * 100

-- Draw robot movements
drawWarehouse :: Warehouse -> IO ()
drawWarehouse w = mapM_ drawRow [0..(height w)-1] 
    where
    drawRow y           = putStrLn $ map (coordToSymbol y) [0..(width w)-1] 
    coordToSymbol y x   = case find (\(xt, yt, _) -> x == xt && y == yt ) (tiles w) of
        Nothing             -> '.'
        Just (_, _, Wall)   -> '#'
        Just (_, _, Box)    -> 'O'
        Just (_, _, BoxL)   -> '['
        Just (_, _, BoxR)   -> ']'
        Just (_, _, Robot)  -> '@'

-- Draws the ware house animation
drawWarehouseAnim :: Float -> Warehouse -> [Move] -> IO ()
drawWarehouseAnim t w ms = do
    setCursorPosition 0 0
    drawWarehouse w
    sleep t
    case ms of
        []        -> return ()
        otherwise -> do
            putStrLn ""
            putStrLn $ "Moved : " ++ show (head ms) ++ " - " ++ show (length (tail ms)) ++ " left"
            drawWarehouseAnim t w' (tail ms)
    where
        w' = stepOne (robotPosition w) (head ms) w

-- The solver for part #1 of the puzzle
solvePart1 :: (Warehouse, [Move]) -> Int
solvePart1 (w, ms) = sum cs 
    where
        cs = map gpsCoordinates (boxPositions w')
        w' = stepMany ms w

-- The solver for part #2 of the puzzle
solvePart2 :: (Warehouse, [Move]) -> Int
solvePart2 (w, ms) = sum cs 
    where
        cs  = map gpsCoordinates (boxPositions w'')
        w'' = stepMany ms w'
        w'  = expandWarehouse w
         
-- The full solver
day15Solver :: IO [Int]
day15Solver = do
    input <- readInputs

    -- Part 1
    let w  = fst input
    let ms = snd input
    clearScreen
    drawWarehouseAnim 0.0 w ms

    -- Part 1
    let w' = expandWarehouse w
    clearScreen
    drawWarehouseAnim 0.0 w' ms

    return [solvePart1 input, solvePart2 input]
