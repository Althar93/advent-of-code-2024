module Day14 (day14Solver) where

import Parser

import Data.Char
import Data.Ord
import Data.List

import System.IO
import System.Console.ANSI
import System.Sleep

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day14_test_input.txt"

-- A 2D int vector
type Vector2Int = (Int, Int)

-- A robot
type Robot = (Vector2Int, Vector2Int)

-- The input file path
inputFile :: FilePath
inputFile = "res/day14_input.txt"

-- Parses a single robot
parseRobot :: Parser Robot
parseRobot = do
    _ <- parseSpaces
    _ <- parseString "p=" 
    px <- parseInt
    _ <- parseChar ','
    py <- parseInt
    _ <- parseSpaces
    _ <- parseString "v=" 
    vx <- parseInt
    _ <- parseChar ','
    vy <- parseInt
    pure $ ((px, py), (vx, vy))

-- Parses multiple robots
parseRobots :: Parser [Robot]
parseRobots = some parseRobot

-- Reads the test input
readInputs :: IO [Robot]
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseRobots contents

-- Moves a single robot
moveRobot :: Int -> Int -> Int -> Robot -> Robot
moveRobot w h n ((px0, py0), (vx, vy)) = ((px1, py1), (vx, vy))
    where
        px1 = (px0 + vx * n) `mod` w
        py1 = (py0 + vy * n) `mod` h

-- Moves all robots
moveRobots :: Int -> Int -> Int -> [Robot] -> [Robot]
moveRobots w h n rs = map (moveRobot w h n) rs

-- The solver for part #1 of the puzzle
solvePart1 :: [Robot] -> Int
solvePart1 rs = product $ map length [tl,tr,bl,br]
    where
        -- Quadrants
        tl          = filter (\((px, py), _) -> px < wh && py < hh) rs'
        tr          = filter (\((px, py), _) -> px > wh && py < hh) rs'
        bl          = filter (\((px, py), _) -> px < wh && py > hh) rs'
        br          = filter (\((px, py), _) -> px > wh && py > hh) rs'
        -- Simulate
        rs'         = moveRobots w h 100 rs
        (wh, hh)    = (w `div` 2, h `div` 2)
        -- Context
        (w, h)      = (101, 103)

-- Draw robot movements
drawRobots :: Int -> Int -> [Robot] -> IO ()
drawRobots w h rs = mapM_ drawRow [0..h-1] 
    where
    drawRow y = putStrLn $ map (coordToSymbol y) [0..w-1] 
        where
        coordToSymbol y x = case (length $ filter (\((px, py), _) -> px == x && py == y) rs) of
            0 -> '.'
            x -> intToDigit x

-- Animated robot drawing
drawRobotsN :: Int -> Int -> Int -> Int -> Int -> Float -> [Robot] -> IO ()
drawRobotsN w h n nM k t rs = do
    case n >= nM of
        True  -> return ()
        False -> case isTree rs' of
            True  -> drawRobots w h rs'
            False -> drawRobotsN w h (n + k) nM k t rs
    where
        rs' = moveRobots w h n rs

-- Returns whether is a tree - assumes no robots overlap
isTree :: [Robot] -> Bool
isTree rs = all (\x -> length x == 1) ns
    where
        ns = map (findOverlap rs) rs

-- Find neighbours
findOverlap :: [Robot] -> Robot -> [Robot]
findOverlap rs ((px0, py0), _) = filter (\((px, py), _) -> px0 == px && py0 == py) rs

-- The solver for part #2 of the puzzle
solvePart2 :: [Robot] -> Int
solvePart2 rs = 0
         
-- The full solver
day14Solver :: IO [Int]
day14Solver = do
    input <- readInputs
    -- Through trial and error found that after 39 iterations a vertical pattern repeats every 101 iterations
    -- Eventually the christmas tree appears after 7412 iterations
    drawRobotsN 101 103 39 10000 101 0.1 input
    return [solvePart1 input, solvePart2 input]