module Day1 (day1Solver) where

import Parser
import Data.List (sort)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day1_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day1_input.txt"

-- A list of value
type LocationID = ([Int],[Int])

-- Parse a single pair
parsePair :: Parser (Int, Int)
parsePair = do
    parseSpaces
    a <- parseInt
    parseSpaces
    b <- parseInt
    pure (a, b)

-- Parses the location ID
parseLocationID :: Parser ([Int], [Int])
parseLocationID = do 
    xs <- some $ parsePair
    pure $ unzip xs

-- Reads the test input
readInputs :: IO LocationID
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseLocationID contents

-- The solver for part #1 of the puzzle
solvePart1 :: LocationID -> Int
solvePart1 xs = sum ds where
    ds           = zipWith (\a b -> abs(b - a)) minS maxS
    minS         = sort $ fst xs
    maxS         = sort $ snd xs

-- The solver for part #2 of the puzzle
solvePart2 :: LocationID -> Int
solvePart2 xs = sum ds where
    ds           = zipWith (*) left right'
    right'       = map (similar right) left
    right        = snd xs
    left         = fst xs
    similar ys y = length $ filter (==y) ys

-- The full solver
day1Solver :: IO [Int]
day1Solver = do
    input <- readInputs
    putStrLn $ show input
    return [solvePart1 input, solvePart2 input]
