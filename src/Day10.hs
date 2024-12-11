module Day10 (day10Solver) where

import Data.List
import Data.Char
import Data.Maybe
import Data.Ord

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day10_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day10_input.txt"

-- Reads the test input
readInputs :: IO [[Int]]
readInputs = do
    contents <- readFile inputFile
    return $ map (map digitToInt) (lines contents)

-- Returns nodes of the given height
trailNodes :: Int -> [[Int]] -> [(Int, Int)]
trailNodes h xs = [(x, y) | x <- [0..length (head xs) -1], y <- [0..length xs - 1], (xs !! y) !! x == h]

-- Returns the list of trail heads
trailHeads :: [[Int]] -> [(Int, Int)]
trailHeads xs = trailNodes 0 xs

-- Returns the list of trail ends
trailEnds :: [[Int]] -> [(Int,Int)]
trailEnds xs = trailNodes 9 xs

-- Returns the longest hike path from head to end
hikeLongest :: [[Int]] -> (Int, Int) -> (Int,Int) -> [(Int, Int)]
hikeLongest xss s e = hike' e [s]
    where
        w = length (head xss)
        h = length xss
        hike' e (x:xs) | x == e = reverse (x:xs)
                       | otherwise = case trail w h xss x of
                          [] -> []
                          ys -> case sortBy (comparing length) [hike' e (y:x:xs) | y <- ys] of
                              []    -> []
                              as    -> last as -- Interested in the longest path

-- Returns all hike paths from head to end
hikeAll :: [[Int]] -> (Int, Int) -> (Int,Int) -> [[(Int, Int)]]
hikeAll xss s e = hike' e [s]
    where
        w = length (head xss)
        h = length xss
        hike' e (x:xs) | x == e = [reverse (x:xs)]
                       | otherwise = case trail w h xss x of
                          [] -> []
                          ys -> concat [hike' e (y:x:xs) | y <- ys] -- Interested in ALL paths
                    
-- Returns all possible trails for a given node
trail :: Int -> Int -> [[Int]] -> (Int, Int) -> [(Int, Int)]
trail w h xss (x0, y0) = [(x, y) | x <- [0..w - 1], y <- [0..h - 1], (abs(x-x0) + abs(y-y0)) == 1, ((xss !! y0) !! x0) + 1 == ((xss !! y) !! x)]

-- The solver for part #1 of the puzzle
solvePart1 :: [[Int]] -> Int
solvePart1 xss = length $ filter (not.null) ys
    where 
        ys = [hikeLongest xss s e | s <- hs, e <- es]
        hs = trailHeads xss
        es = trailEnds xss

-- The solver for part #2 of the puzzle
solvePart2 :: [[Int]] -> Int
solvePart2 xss = sum $ map length ys
    where 
        ys = [hikeAll xss s e | s <- hs, e <- es]
        hs = trailHeads xss
        es = trailEnds xss

-- The full solver
day10Solver :: IO [Int]
day10Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
