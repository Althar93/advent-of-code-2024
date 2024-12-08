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
-- One operator
type Operator = (Int -> Int -> Int)

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
            pure $ x

-- Parses multiple equations
parseEquations :: Parser [Equation]
parseEquations = some parseEquation

-- Reads the test input
readInputs :: IO [Equation]
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseEquations contents

-- Evaluates the given sequence of numbers and operators
evaluate :: [Int] -> [Operator] -> Int
evaluate (x:y:xs) (f:fs) = evaluate ((f x y):xs) fs
evaluate (x:_) _         = x
evaluate _ _             = error "Not handled"

-- Returns the results of an equation
result :: Equation -> Int
result = fst

-- Concatenates two numbers
concatenate :: Int -> Int -> Int
concatenate x y = read $ (show x) ++ (show y)

-- Returns all possible solutions (for part 1)
solutions :: Equation -> [[Operator]]
solutions (y0, xs0) = solutions' (y0, xs0) [] where
    solutions' (y, xs) os       | y' > y                     = []
                                | length os == length xs - 1 = if y' == y then [os] else []
                                | otherwise                  = mul ++ add
                                where 
                                    y'  = evaluate xs (reverse os) -- #TODO : Not sure why reversing the operators is necessary here to get the correct solution?
                                    mul = solutions' (y, xs) ((*):os)
                                    add = solutions' (y, xs) ((+):os)

-- Returns all possible solutions (for part 2)
solutionsCon :: Equation -> [[Operator]]
solutionsCon (y0, xs0) = solutionsCon' (y0, xs0) [] where
    solutionsCon' (y, xs) os    | y' >  y                      = []
                                | length os == length xs - 1   = if y' == y then [os] else []
                                | otherwise                    = con ++ mul ++ add
                                where 
                                    y'  = evaluate xs (reverse os) -- #TODO : Not sure why reversing the operators is necessary here to get the correct solution?
                                    mul = solutionsCon' (y, xs) ((*):os)
                                    add = solutionsCon' (y, xs) ((+):os)
                                    con = solutionsCon' (y, xs) ((concatenate):os)

-- The solver for part #1 of the puzzle
solvePart1 :: [Equation] -> Int
solvePart1 es = sum (map result es') 
    where
        es' = filter (\e -> length (solutions e) > 0) es

-- The solver for part #2 of the puzzle
solvePart2 :: [Equation]  -> Int
solvePart2 es = sum (map result es') 
    where
        es' = filter (\e -> length (solutionsCon e) > 0) es

-- The full solver
day7Solver :: IO [Int]
day7Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]