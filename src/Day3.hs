module Day3 (day3Solver) where

import Parser
import Data.Maybe

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day3_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day3_input.txt"

-- A report
data Instruction = Mul (Int, Int) | Do | Dont deriving Show

-- Parses do
parseDo :: Parser Instruction
parseDo = do 
    parseString "do()"
    pure $ Do

-- Parses don't
parseDont :: Parser Instruction
parseDont = do
    parseString "don't()"
    pure $ Dont

-- Parse mul
parseMul :: Parser Instruction
parseMul = do 
    parseString "mul("
    x <- parseInt
    parseChar ','
    y <- parseInt
    parseString ")"
    pure $ Mul (x, y)

-- Parses a possible instruction
parseMInstruction :: Parser (Maybe Instruction)
parseMInstruction = do
    parseSpaces
    ns <- pEither (parseMul <|> parseDo <|> parseDont) parseItem
    case ns of
        Left n  -> pure $ Just n
        Right _ -> pure $ Nothing

-- Parses the memory
parseMemory :: Parser [Instruction]
parseMemory = do
    xs <- some parseMInstruction
    pure $ catMaybes xs

-- Returns whether an instruction is a mul
isMul :: Instruction -> Bool
isMul (Mul (_, _)) = True
isMul _            = False

-- Reduces a set of instructions based on do's and don'ts
reduce :: Bool -> [Instruction] -> [Instruction]
reduce _ []        = []
reduce b (x:xs)    = case x of
    Dont -> reduce False xs
    Do   -> reduce True  xs
    m    -> if b then m : (reduce b xs) else (reduce b xs)

-- Reads the test input
readInputs :: IO [Instruction]
readInputs = do
    contents <- readFile inputFile
    pure $ runParser parseMemory contents

-- The solver for part #1 of the puzzle
solvePart1 :: [Instruction] -> Int
solvePart1 xs = sum $ map (\(Mul (a, b)) -> a * b) xs' where
    xs' = filter isMul xs

-- The solver for part #2 of the puzzle
solvePart2 :: [Instruction] -> Int
solvePart2 xs = sum $ map (\(Mul (a, b)) -> a * b) xs' where
    xs'' = filter isMul xs'
    xs'  = reduce True xs

-- The full solver
day3Solver :: IO [Int]
day3Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
