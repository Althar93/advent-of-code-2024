module Day8 (day8Solver) where

import Parser
import Data.List

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day8_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day8_input.txt"

-- An antenna consisting of a frequency & position
type Antenna = (Int, Int, Char)

-- A map
type Map = ([Antenna], Int, Int)

processMap :: [[Char]] -> Map
processMap xss = (xs, xMax, yMax) 
    where
        xs   = [(x, y, (xss !! y) !! x) | x <- [0..xMax], y <- [0..yMax],  ((xss !! y) !! x) /= '.' ]
        xMax = length (head xss) - 1
        yMax = length xss - 1

-- Reads the test input
readInputs :: IO Map
readInputs = do
    contents <- readFile inputFile
    pure $ processMap (lines contents)

-- Returns the antinode for two given nodes
antinodes :: Int -> Int -> [Int] -> Antenna -> Antenna -> [Antenna]
antinodes xMax yMax ss (xa, ya, a) (xb, yb, b) | a /= b    = []
                                                | otherwise = left ++ right
                                                where
                                                    left     = takeWhile (inRange xMax yMax) [(xa - xu * s, ya - yu * s, '#') | s <- ss]
                                                    right    = takeWhile (inRange xMax yMax) [(xb + xu * s, yb + yu * s, '#') | s <- ss]
                                                    (xu, yu) = (xb - xa, yb - ya)

-- Returns the frequency of an antenna
frequency :: Antenna -> Char
frequency (_, _, c) = c

-- Returns whether an antenna is in range
inRange :: Int -> Int -> Antenna -> Bool
inRange xMax yMax (x, y, _) = x >= 0 && x <= xMax && y >= 0 && y <= yMax

-- Generates pairs of antennas
pairs :: [Antenna] -> [(Antenna, Antenna)]
pairs xs = nub [(x,y) | x <- xs, y <- xs, x < y, frequency x == frequency y]

-- Returns a list with unique elements
unique :: Eq a => [a] -> [a]
unique = reverse . nub . reverse

-- The solver for part #1 of the puzzle
solvePart1 :: Map -> Int
solvePart1 (xs, xMax, yMax) = length as'
    where
        as' = unique as
        as  = concatMap (\(a, b) -> antinodes xMax yMax [1] a b) ps
        ps  = pairs xs
        xs' = map (\(x, y, _) -> (x, y)) xs
    
-- The solver for part #2 of the puzzle
solvePart2 :: Map  -> Int
solvePart2 (xs, xMax, yMax) = length as'
    where
        as' = unique as
        as  = concatMap (\(a, b) -> antinodes xMax yMax [0..] a b) ps
        ps  = pairs xs
        xs' = map (\(x, y, _) -> (x, y)) xs

-- The full solver
day8Solver :: IO [Int]
day8Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]