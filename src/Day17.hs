module Day17 (day17Solver) where

import Parser
import Data.List
import Data.Bits

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day17_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day17_input.txt"

-- Operands
data Operand = Zero | One | Two | Three | RegA | RegB | RegC deriving (Show, Eq, Enum)

-- Op codes
data OpCode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show, Eq, Enum)

-- Registers
type Registers = (Int, Int, Int)

-- A computer
type Computer = (Registers, [Int])

parseRegisters :: Parser Registers
parseRegisters = do
    _ <- parseSpaces
    _ <- parseString "Register A: "
    a <- parseInt
    _ <- parseSpaces
    _ <- parseString "Register B: "
    b <- parseInt
    _ <- parseSpaces
    _ <- parseString "Register C: "
    c <- parseInt
    return (a, b, c)

-- Parses instructions
parseInstructions :: Parser [Int]
parseInstructions = do
    _ <- parseSpaces
    _ <- parseString "Program: "
    is <- some $ parseMore <|> parseInt
    return is
    where
        parseMore = do
            i <- parseInt
            parseChar ','
            return i

-- Parses a computer
parseComputer :: Parser Computer
parseComputer = do
    rs <- parseRegisters
    is <- parseInstructions
    return (rs, is)

-- Reads the test input
readInputs :: IO Computer
readInputs = do
    contents <- readFile testInputFile
    pure $ runParser parseComputer contents

-- Evaluates the operand
evalOperand :: Registers -> Operand -> Int
evalOperand _ Zero       = 0
evalOperand _ One        = 1
evalOperand _ Two        = 2
evalOperand _ Three      = 3
evalOperand (a,_,_) RegA = a
evalOperand (_,b,_) RegB = b
evalOperand (_,_,c) RegC = c

-- Executes the computer
runComputer :: Computer -> Int -> [Int] -> [Int]
runComputer ((a,b,c), is) ip rs     | ip >= (length is - 1) = reverse rs                                                                          -- Reached the end of the program
                                    | otherwise             = case toEnum (is !! ip) of
                                                                Adv -> runComputer ((a `div` 2^o, b, c), is) (ip + 2) rs                          -- Division A
                                                                Bxl -> runComputer ((a, b `xor` l, c), is)   (ip + 2) rs                          -- XOR
                                                                Bst -> runComputer ((a, o `mod` 8, c), is)   (ip + 2) rs                          -- Mod 8
                                                                Jnz -> runComputer ((a, b, c), is)           (if a == 0 then ip + 2 else l) rs    -- Jump
                                                                Bxc -> runComputer ((a, b `xor` c, c), is)   (ip + 2) rs                          -- XOR
                                                                Out -> runComputer ((a, b, c), is)           (ip + 2) (r:rs)                      -- Return
                                                                Bdv -> runComputer ((a, a `div` 2^o, c), is) (ip + 2) rs                          -- Division B
                                                                Cdv -> runComputer ((a, b, a `div` 2^o), is) (ip + 2) rs                          -- Division C
                                                                where 
                                                                    o = evalOperand (a, b, c) (toEnum l)                                          -- Combo operand
                                                                    l = (is !! (ip + 1))                                                          -- Literal operand
                                                                    r = (o `mod` 8)                                                               -- Return value

--reverseComputer :: Computer -> Int -> [Int] -> Int
--reverseComputer ((a,b,c), is) ip rs     | ip == 0 && null rs    = a
--                                        | otherwise             = case toEnum (is !! ip - 1) of
--                                                                    Adv -> error "Not implemented"
--                                                                    Bxl -> reverseComputer ((a, b `xor` l, c), is)   (ip - 2) rs                          -- XOR
--                                                                    Bst -> reverseComputer ((a, o, c), is)           (ip - 2) rs                          -- Mod 8
--                                                                    Jnz -> reverseComputer ((a, b, c), is)           (if a == 0 then ip - 2 else -l) rs   -- Jump
--                                                                    Bxc -> reverseComputer ((a, b `xor` c, c), is)   (ip - 2) rs                          -- XOR
--                                                                    Out -> reverseComputer ((a, b, c), is)           (ip - 2) (init rs)                   -- Return
--                                                                    Bdv -> error "Not implemented"
--                                                                    Cdv -> error "Not implemented"
--                                                                    where
--                                                                        o = r -- Can be ANY value between [0;7]
--                                                                        l = (is !! (ip - 1))  
--                                                                        r = last rs

-- The solver for part #1 of the puzzle
solvePart1 :: Computer -> [Int]
solvePart1 xss = runComputer xss 0 []

-- The solver for part #2 of the puzzle
solvePart2 :: Computer -> [Int]
solvePart2 xss = if null cs then [] else [head cs] --runComputer xss' 0 []
    where
        cs      = [a  | a <- [0..2000000], let c = runComputer ((a, 0, 0), is) 0 [], c == is]
        is = snd xss
        --xss'    = ((117440, 0, 0), is)
    --    is = snd xss
         
-- The full solver
day17Solver :: IO [[Int]]
day17Solver = do
    input <- readInputs
    putStrLn $ show input
    return [solvePart1 input, solvePart2 input]
