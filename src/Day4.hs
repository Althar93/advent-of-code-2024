module Day4 (day4Solver) where

import Parser
import Data.Maybe
import Data.List

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day4_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day4_input.txt"

-- Reads the test input
readInputs :: IO [[Char]]
readInputs = do
    contents <- readFile inputFile
    return $ lines contents

-- List of orthogonal neighbours
neighboursT :: Int -> Int -> Int -> (Int, Int) -> [[(Int, Int)]]
neighboursT w h n (x0, y0) = [l] ++ [r] ++ [u] ++ [d] where
    l  = [(x0 - s, y0)     | s <- [0..n - 1]]
    r  = [(x0 + s, y0)     | s <- [0..n - 1]]
    u  = [(x0, y0 - t)     | t <- [0..n - 1]]
    d  = [(x0, y0 + t)     | t <- [0..n - 1]]

-- List of diagonal neighbours 
neighboursX :: Int -> Int -> Int -> (Int, Int) -> [[(Int, Int)]]
neighboursX w h n (x0, y0) = [ul] ++ [ur] ++ [dl] ++ [dr] where
    ul = [(x0 - t, y0 - t) | t <- [0..n - 1]]
    ur = [(x0 + t, y0 - t) | t <- [0..n - 1]]
    dl = [(x0 - t, y0 + t) | t <- [0..n - 1]]
    dr = [(x0 + t, y0 + t) | t <- [0..n - 1]]

-- All neighbours (orthogonal & diagonal)
neighbours ::  Int -> Int -> Int -> (Int, Int) -> [[(Int, Int)]]
neighbours w h n p = (neighboursT w h n p) ++ (neighboursX w h n p)

-- Returns the width of a 2D list
width :: [[a]] -> Int
width = length . head

-- Returns the height of a 2D list
height :: [[a]] -> Int
height = length

-- Return the 
at :: [[a]] -> (Int, Int) -> Maybe a
xss `at` (x, y) | x < 0 || x >= length (head xss) = Nothing
                | y < 0 || y >= length xss        = Nothing
                | otherwise                       = Just $ (xss !! y) !! x

-- Matches a given sequence
matchSequence :: [[Char]] -> String -> [(Int, Int)] -> Bool
matchSequence _   [] []               = True
matchSequence xss (c0:cs) ((x, y):ys) = case xss `at` (x, y) of
                                            Nothing -> False
                                            Just c  -> if c == c0 then matchSequence xss cs ys else False

-- The solver for part #1 of the puzzle
solvePart1 :: [[Char]] -> Int
solvePart1 xss = length ys' where
    ys' = filter (==True) ys
    ys  = map (matchSequence xss "XMAS") ns
    ns  = concatMap (neighbours w h 4) [(x, y) | x <- [0..w - 1], y <- [0..h - 1], xss `at` (x, y) == Just 'X']
    w   = width xss
    h   = height xss

-- The solver for part #2 of the puzzle
solvePart2 :: [[Char]] -> Int
solvePart2 xss = length ys''' `div` 2 where
    ys''' = concat ys''
    ys''  = filter (\a -> length a > 1) ((group . sort) ys')
    ys'   = map (!!1) ys
    ys    = catMaybes mYs
    mYs   = map (\n -> if matchSequence xss "MAS" n then Just n else Nothing) ns
    ns    = concatMap (neighboursX w h 3) [(x, y) | x <- [0..w - 1], y <- [0..h - 1], xss `at` (x, y) == Just 'M']
    w     = width xss
    h     = height xss

-- The full solver
day4Solver :: IO [Int]
day4Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
