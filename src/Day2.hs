module Day2 (day2Solver) where

import Parser

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day2_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day2_input.txt"

-- A report
type Report = [Int]

-- Parse reports
parseReport :: Parser Report
parseReport = do 
    parseSpaces
    ns <- some $ parseMore <|> parseInt 
    return ns
    where
        parseMore = do
            n <- parseInt 
            parseChar ' '
            return n

-- Parses multiple reports
parseReports :: Parser [Report]
parseReports = some parseReport

-- Reads the test input
readInputs :: IO [Report]
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseReports contents

-- Returns if a report is safe
isSafe :: Report -> Bool
isSafe xs = all (\y -> signum y == signum (head ys) && abs y >= 1 && abs y <= 3) ys
    where
        ys = zipWith (-) (tail xs) (init xs)

-- Returns if a report is safe (with a margin for error)
isSafe' :: Report -> Bool
isSafe' xs = any isSafe (xs : [deleteAt n xs | n <- [0.. length xs - 1]]) 
    where
        deleteAt 0 (x:xs) = xs
        deleteAt n (x:xs) = x : deleteAt (n - 1) xs
            
-- The solver for part #1 of the puzzle
solvePart1 :: [Report] -> Int
solvePart1 xs = length $ filter isSafe xs

-- The solver for part #2 of the puzzle
solvePart2 :: [Report] -> Int
solvePart2 xs = length $ filter isSafe' xs

-- The full solver
day2Solver :: IO [Int]
day2Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
