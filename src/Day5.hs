module Day5 (day5Solver) where

import Parser
import Data.List

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day5_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day5_input.txt"

-- Data types
type OrderingRule = (Int, Int)
type Manual       = ([OrderingRule], [[Int]])

-- Parses an ordering rule
parseOrderingRule :: Parser OrderingRule
parseOrderingRule = do
    parseSpaces
    a <- parseInt
    parseChar '|'
    b <- parseInt
    pure (a, b)

-- Parses pages to produce
parsePagesToProduce :: Parser [Int]
parsePagesToProduce = do
    parseSpaces
    ps <- some $ parseMore <|> parseInt 
    pure ps
    where
        parseMore = do
            p <- parseInt 
            parseChar ','
            pure p

-- Parses the manual
parseManual :: Parser Manual
parseManual = do
    os  <- some parseOrderingRule
    parseSpaces
    pss <- some parsePagesToProduce
    pure (os, pss)

-- Reads the test input
readInputs :: IO Manual
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseManual contents

-- Ordering function
ordering :: [OrderingRule] -> Int -> Int -> Ordering
ordering os a b = case find (\(u, v) -> (a == u && b == v) || (a == v && b == u)) os of
    Nothing     -> EQ
    Just (u, v) -> if a == u then LT else GT

-- Returns whether a page has the correct ordering
isCorrectOrder :: [OrderingRule] -> [Int] -> Bool
isCorrectOrder os xs = xs == xs' where
    xs' = sortBy (ordering os) xs

-- The solver for part #1 of the puzzle
solvePart1 :: Manual -> Int
solvePart1 m = sum ps'' where
    ps'' = map (\ys -> ys !! ((length ys) `div` 2)) ps'
    ps'  = filter (isCorrectOrder os) ps
    ps   = snd m
    os   = fst m

-- The solver for part #2 of the puzzle
solvePart2 :: Manual -> Int
solvePart2 m = sum ps''' where
    ps''' = map (\ys -> ys !! ((length ys) `div` 2)) ps''
    ps''  = map (sortBy (ordering os)) ps'
    ps'   = filter (not . (isCorrectOrder os)) ps
    ps    = snd m
    os    = fst m

-- The full solver
day5Solver :: IO [Int]
day5Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
