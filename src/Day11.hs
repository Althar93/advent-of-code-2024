module Day11 (day11Solver) where

import Parser
import Data.List

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day11_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day11_input.txt"

-- Parses stones
parseStones :: Parser [Int]
parseStones = some parseStone 
    where
        parseStone = do
            parseSpaces
            parseInt

-- Reads the test input
readInputs :: IO [Int]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseStones contents

-- Blinks the lights once
blinkOnce :: [Int] -> [Int]
blinkOnce [] = []
blinkOnce (x:xs)    | x == 0 = 1 : blinkOnce xs
                    | even (length (show x)) = (splitStone x) ++ blinkOnce xs
                    | otherwise = (x * 2024) : blinkOnce xs

-- Alternate blink for much faster iteration
blinkOnce' :: [(Int, Int)] -> [(Int, Int)]
blinkOnce' [] = []
blinkOnce' ((x, n):xs) = case blinkStone x of
    [y] -> (y, n):blinkOnce' xs
    [y, z] -> (y, n):(z, n):blinkOnce' xs

blink' :: Int -> [(Int, Int)] -> [(Int,Int)]
blink' 0 xs = xs
blink' n xs = blink' (n-1) xs2
    where 
        xs2 = reduce xs1
        xs1 = (blinkOnce' xs)

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce xs = map (\y -> ((fst.head) y, sum ((map snd) y))) xs1 
    where 
        xs1 = ((groupBy (\a b -> fst a == fst b)) . sort) xs
        
-- Blinks a single stone
blinkStone :: Int -> [Int]
blinkStone x    | x == 0 = [1]
                | even (length (show x)) = splitStone x
                | otherwise = [x * 2024]

-- Split stone into two
splitStone :: Int -> [Int]
splitStone x = [read left, read right]
    where
        (left, right) = splitAt (div (length x') 2) x'
        x' = show x

-- Blinks the lights a number of times
blink :: Int -> [Int] -> [Int]
blink 0 xs = xs
blink n xs = blink (n - 1) (blinkOnce xs)

-- The solver for part #1 of the puzzle
solvePart1 :: [Int] -> Int
solvePart1 xs = length $ blink 25 xs

-- The solver for part #2 of the puzzle
solvePart2 :: [Int] -> Int
solvePart2 xs = sum $ map snd xs3
    where
        xs3 = blink' 75 xs2
        xs2  = map (\ys -> (head ys, length ys)) xs1
        xs1  = (group . sort) xs

-- The full solver
day11Solver :: IO [Int]
day11Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
