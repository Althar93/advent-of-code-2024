module Day9 (day9Solver) where

import Data.Char
import Data.Maybe

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day9_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day9_input.txt"

-- A single memory block 
type MemoryBlock = (Maybe Int, Int)

-- Processes memory into sixe & free space chunks
processMemory :: Int -> [Char] -> [MemoryBlock]
processMemory n (l:e:cs)  = [(Just n, digitToInt l)] ++ [(Nothing, digitToInt e)] ++ processMemory (n + 1) cs
processMemory n (l:[])    = [(Just n, digitToInt l)] 
processMemory _ []        = []

-- Defragments the memory blocks for part 1
defragment :: Int -> [MemoryBlock] -> [MemoryBlock]
defragment _ [] = []
defragment n (x:xs) = case x of
    (Just _,  _ ) -> x  : defragment n xs                                   -- Defragmented block
    (Nothing, ke) -> case (last xs) of
        (Nothing, _)  -> defragment (n + 1) (x : (init xs))                 -- Trim nothing
        (Just nl, kl) -> x' : defragment (n + 1) (e' ++ (init xs) ++ l')         
            where
                x'  = (Just nl, min ke kl)                                  -- Move block
                e'  = if ke > kl then [(Nothing, ke - kl)] else []          -- Remaining empties
                l'  = if kl > km then [(Just nl, kl - km)] else []          -- Remaining blocks
                km  = min ke kl                                             -- Move count

-- Defragments the memory blocks for part 2
defragment' :: Int -> [MemoryBlock] -> [MemoryBlock]
defragment' _ [] = []
defragment' n xs = case last xs of
    (Nothing, _)  -> defragment' n (init xs) ++ [last xs]
    (Just nl, kl) -> case insertBlock (Just nl, kl) (init xs) of
            Nothing   -> defragment' (n + 1) (init xs) ++ [last xs]
            Just xs'  -> defragment' (n + 1) xs' ++ [(Nothing, kl)]

-- Inserts a block
insertBlock :: MemoryBlock -> [MemoryBlock] -> Maybe [MemoryBlock]
insertBlock (n, k) xs = insertBlock' [] xs 
    where
        insertBlock' _ []                               = Nothing
        insertBlock' ys ((Just nx, kx):xs)              = insertBlock' ((Just nx, kx):ys) xs
        insertBlock' ys ((Nothing, ke):xs)  | k == ke   = Just $ reverse ys ++ (n, k) : xs      
                                            | k <  ke   = Just $ reverse ys ++ (n, k) : (Nothing, ke - k) : xs
                                            | k >  ke   = insertBlock' ((Nothing, ke):ys) xs

-- Computes the checksum of a lost of defragmented memory block
checksum :: [MemoryBlock] -> Int
checksum xs = checksum' 0 xs 
    where
        checksum' n (y:ys) = case y of
            (Nothing, ke)    -> checksum' (n + ke) ys 
            (Just nl, kl)    -> (sum [nl * k | k <- [n..n + kl - 1]]) + checksum' (n + kl) ys
        checksum' _ []     = 0

-- Shows a memory block
showMemoryBlock :: [MemoryBlock] -> String
showMemoryBlock []      = ""
showMemoryBlock (x:xs)  = case x of
    (Just n, k)  -> (replicate k (intToDigit n)) ++ showMemoryBlock xs
    (Nothing, k) -> (replicate k '.'           ) ++ showMemoryBlock xs

-- Reads the test input
readInputs :: IO [MemoryBlock]
readInputs = do
    contents <- readFile inputFile
    pure $ processMemory 0 contents

-- The solver for part #1 of the puzzle
solvePart1 :: [MemoryBlock] -> Int
solvePart1 xs = checksum $ defragment 0 xs
    
-- The solver for part #2 of the puzzle
solvePart2 :: [MemoryBlock]  -> Int
solvePart2 xs = checksum $ defragment' 0 xs

-- The full solver
day9Solver :: IO [Int]
day9Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]