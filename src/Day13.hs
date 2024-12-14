module Day13 (day13Solver) where

import Parser
import Data.Maybe

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day13_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day13_input.txt"

data Machine = Machine {
    buttonA :: !(Integer, Integer),
    buttonB :: !(Integer, Integer),
    prize   :: !(Integer, Integer)
} deriving (Show)

-- Parses a machine
parseMachine :: Parser Machine
parseMachine = do
    (xa, ya) <- parseButton "A"
    (xb, yb) <- parseButton "B"
    (xp, yp) <- parsePrize
    pure $ Machine { buttonA = (xa, ya), buttonB = (xb, yb), prize = (xp, yp) }
    where
        parseButton c = do
            parseSpaces
            _ <- parseString $ "Button " ++ c ++ ": X+"
            x <- parseInt
            _ <- parseString ", Y+"
            y <- parseInt
            pure $ (fromIntegral x, fromIntegral y)
        parsePrize = do
            parseSpaces
            _ <- parseString "Prize: X="
            x <- parseInt
            _ <- parseString ", Y="
            y <- parseInt
            pure $ (fromIntegral x, fromIntegral y)

-- Parses multiple machines
parseMachines :: Parser [Machine]
parseMachines = some parseMachine

-- Solves a machine & returns both solutions (for part 1)
solve :: Integer -> Machine -> Maybe (Integer, Integer)
solve n m   | du == dv                                    = Nothing                  -- No possible solution
            | (round a) * ua + (round b) * ub /= x        = Nothing                  -- Imperfect solution
            | (round a) * va + (round b) * vb /= y        = Nothing                  -- Imperfect solution
            | a > fromIntegral n || b > fromIntegral n    = Nothing                  -- Pressed too many times
            | otherwise                                   = Just (round a, round b)  -- One solution
            where
                b        = -a * du + cu                          :: Float
                a        = (cv - cu) / (dv - du)                 :: Float
                du       = (fromIntegral ua) / (fromIntegral ub) :: Float
                dv       = (fromIntegral va) / (fromIntegral vb) :: Float
                cu       = (fromIntegral x)  / (fromIntegral ub) :: Float
                cv       = (fromIntegral y)  / (fromIntegral vb) :: Float
                (ua, va) = buttonA m
                (ub, vb) = buttonB m
                (x, y)   = prize m

-- Solves a machine & returns both solutions (for part 2)
solve' :: Integer -> Machine -> Maybe (Integer, Integer)
solve' n m  | du == dv                                = Nothing                    -- No possible solution
            | (round a') * ua + (round b') * ub /= x' = Nothing                    -- Imperfect solution
            | (round a') * va + (round b') * vb /= y' = Nothing                    -- Imperfect solution
            | otherwise                               = Just (round a', round b')  -- One solution
            where
                -- Translate intersection
                b'       = -a' * du + cu'
                a'       = a + ((fromIntegral n / fromIntegral vb) - (fromIntegral n / fromIntegral ub)) / (dv - du) :: Double
                cu'      = (fromIntegral x + fromIntegral n) / (fromIntegral ub) :: Double
                (x', y') = (x + n, y + n)
                -- Same as before
                b        = -a * du + cu                          :: Double
                a        = (cv - cu) / (dv - du)                 :: Double
                du       = (fromIntegral ua) / (fromIntegral ub) :: Double
                dv       = (fromIntegral va) / (fromIntegral vb) :: Double
                cu       = (fromIntegral x)  / (fromIntegral ub) :: Double
                cv       = (fromIntegral y)  / (fromIntegral vb) :: Double
                (ua, va) = buttonA m
                (ub, vb) = buttonB m
                (x, y)   = prize m

-- Reads the test input
readInputs :: IO [Machine]
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseMachines contents

-- Returns the number of tokens
tokens :: (Integer, Integer) -> Integer
tokens (a, b) = 3 * a + b

-- The solver for part #1 of the puzzle
solvePart1 :: [Machine] -> Integer
solvePart1 ms = sum $ map tokens xs 
    where
        xs = catMaybes $ map (solve 100) ms

-- The solver for part #2 of the puzzle
solvePart2 :: [Machine] -> Integer
solvePart2 ms = sum $ map tokens xs 
    where
        xs = catMaybes $ map (solve' 10000000000000) ms
         
-- The full solver
day13Solver :: IO [Integer]
day13Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
