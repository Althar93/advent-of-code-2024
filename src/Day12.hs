module Day12 (day12Solver) where

import Data.List
import Data.Ord

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day12_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day12_input.txt"

-- The map type
type Map = [[Char]]

-- Reads the test input
readInputs :: IO Map
readInputs = do
    contents <- readFile testInputFile
    return $ lines contents

-- Counts plot area & edges
countPlot :: Map -> Int -> Int -> (Int, Int) -> (Char, Int, Int)
countPlot xss w h (x0, y0) = (plot, area, perimeter)
    where
        area       = 1
        perimeter  = 4 - length adjacents
        adjacents  = [(x, y) | x <- [0..w - 1], y <- [0..h - 1], abs(x - x0) + abs(y - y0) == 1, (xss `at` (x, y)) == plot]
        plot       = ((xss !! y0) !! x0)

-- Flood fills given an adjacency rule, 2D list & starting point
--floodFill :: (a -> a -> Bool) -> [[a]] -> (Int, Int) -> [a]
--floodFill f xss s = floodFill' [s] [xss `at` s]
--    where
--        w = width xss
--        h = height xss
--
--        floodFill' :: [(Int, Int)] -> [a] -> [a]
--        floodFill' (p:ps) (z:zs) = case adjacents of
--            [] -> (z:zs)
--            ys -> [floodFill' (y:p:ps) ((xss `at` y):zs) | y <- ys] ++ zs
--            where
--                adjacents  = [(x1, y1) | x1 <- [0..w - 1], y1 <- [0..h - 1], abs(x1 - x0) + abs(y1 - y0) == 1, (f (xss `at` (x1, y1)) z) && not ((x1, y1) `elem` ps)]
--                (x0, y0)   = p

-- Add plots together
addPlots :: [(Char, Int, Int)] -> (Char, Int, Int)
addPlots []                 = ('.', 0, 0)
addPlots ((pl0, a0, p0):ps) = (pl0, a0 + a1, p0 + p1)
    where
        (_, a1, p1) = addPlots ps

at :: [[a]] -> (Int, Int) -> a
at xss (x, y) = (xss !! y) !! x

-- Width of a 2D list
width :: [[a]] -> Int
width = length . head

-- Height of a 2D list
height :: [[a]] -> Int
height = length

-- Returns the plot
plotId :: (Char, Int, Int) -> Char
plotId (p, _, _) = p

-- The solver for part #1 of the puzzle
solvePart1 :: Map -> Int
solvePart1 xss = error $ show pss' --sum $ map price pss''
    where
        pss'' = map addPlots pss'
        pss'  = groupBy (\a b -> plotId a == plotId b) $ sortBy (comparing plotId) pss
        pss   = [countPlot xss w h (x, y) | x <- [0..w - 1], y <- [0..h - 1]]
        w     = width xss
        h     = height xss 

-- The solver for part #2 of the puzzle
solvePart2 :: Map -> Int
solvePart2 xss = 0

-- Computes the price of a region
price :: (Char, Int, Int) -> Int
price (_, a, p) = a * p

-- The full solver
day12Solver :: IO [Int]
day12Solver = do
    input <- readInputs
    putStrLn $ show input
    let w  = width input
    let h  = height input
    let p1 = ('c', 1, 2)
    let p2 = ('c', 3, 5)
    let p3 = addPlots [p1, p2]
    let s  = countPlot input w h (4,8)
    putStrLn $ show s
    return [solvePart1 input, solvePart2 input]
