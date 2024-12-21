module Day19 (day19Solver) where

import Parser
import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Either

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day19_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day19_input.txt"

type Towel = [Char]

type Order = ([Towel], [Towel])

-- Parses towels
parseTowels :: Parser [Towel]
parseTowels = some $ parseMore <|> parseOne
    where
        parseOne  = some $ parseIs isAlphaNum
        parseMore = do
            _ <- parseString ", "
            t <- parseOne
            return t

parsePatterns :: Parser [Towel]
parsePatterns = some $ parseMore <|> parseOne
    where
        parseOne  = some $ parseIs isAlphaNum
        parseMore = do
            _ <- parseSpaces
            p <- parseOne
            return p

-- Parses orders (pre-sort patterns)
parseOrders :: Parser Order
parseOrders = do
    ts <- parseTowels
    ps <- parsePatterns
    return (ts, ps)

-- Makes a towel pattern from the given towels (for part 1)
makeTowel :: [Towel] -> Towel -> [Towel] -> Either [Towel] Bool
makeTowel ts xs cs  | xs `elem` cs = Right False                                                    -- Already in the closed set
                    | otherwise    = case mapMaybe (`stripPrefix` xs) ts' of                        -- Match prefixes?
                                        []   -> Left (xs:cs)                                        -- => No match ; add to closed set
                                        xss' -> case find null xss' of                              -- Complete towel?
                                            Nothing -> chain [makeTowel ts' xs' | xs' <- xss'] cs   -- => Check other options
                                            Just _  -> Right True           
                                        where 
                                            ts' = filter (\t -> (length t) <= (length xs)) (sort ts)

-- Chains eithers passing the Left result of the first item to the second
chain :: (Show a) => [([a] -> Either [a] b)] -> [a] -> Either [a] b
chain [] a      = Left a
chain (x:xs) a  = case x a of
                    Left a'  -> chain xs (a'++a)
                    Right b  -> Right b

-- Reads the test input
readInputs :: IO Order
readInputs = do
    contents <- readFile testInputFile
    pure $ runParser parseOrders contents

-- The solver for part #1 of the puzzle
solvePart1 :: Order -> Int
solvePart1 (ts, ps) = length $ filter (==True) xs'
    where
        xs' = rights xs
        xs  = map (\p -> makeTowel ts p []) ps

-- The solver for part #2 of the puzzle
solvePart2 :: Order -> Int
solvePart2 o = 0
         
-- The full solver
day19Solver :: IO [Int]
day19Solver = do
    input <- readInputs

    --let a = "rubbgwuwbrwgrgrgrbrbbubgbggrbwrurwuggggwrbrgwbrrwb"
    --let a'  = deleteAll "bb" a
    --let a''  = count "g" a
    --print a'
    --print a''
    --print input

    --let a = "b...."
    --let a' = replaceAll "b" "." a
    --print a'

    let ts = fst input
    let ps = snd input
    let xs = map (\p -> (p, makeTowel ts p [])) ps

    --mapM_ print ps
    mapM_ print xs

    --let i  = 5
    --let t1 = makeTowel 0 ts (ps !! i) []
    --print (ps !! i)
    --print t1

    return [solvePart1 input, solvePart2 input]

-- 256 too low
-- 279 not the right answer
-- 400 too high