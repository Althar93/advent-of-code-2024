module Day7 (day7Solver) where

import Parser

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day7_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day7_input.txt"

-- One equation
type Equation = (Int, [Int])

-- Parses an equation
parseEquation :: Parser Equation
parseEquation = do
    parseSpaces
    y <- parseInt
    parseString ": "
    xs <- some $ parseMore <|> parseInt
    pure (y, xs)
    where
        parseMore = do
            x <- parseInt 
            parseChar ' '
            pure x

-- Parses multiple equations
parseEquations :: Parser [Equation]
parseEquations = some parseEquation

-- Reads the test input
readInputs :: IO [Equation]
readInputs = do
    contents <- readFile testInputFile
    pure $ runParser parseEquations contents

-- The solver for part #1 of the puzzle
solvePart1 :: a -> Int
solvePart1 p = 0

-- The solver for part #2 of the puzzle
solvePart2 :: a -> Int
solvePart2 p = 0

-- The full solver
day7Solver :: IO [Int]
day7Solver = do
    input <- readInputs
    putStrLn $ show input
    return [solvePart1 input, solvePart2 input]
