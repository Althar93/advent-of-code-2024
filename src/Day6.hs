module Day6 (day6Solver) where

import Data.List
import Data.Either
import qualified Data.Set 

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day6_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day6_input.txt"

-- A position
type Position = (Int, Int)
-- A direction type
type Direction = (Int, Int)
-- A patrol type
type Patrol = (Move, [Position])
-- A move
type Move   = (Position, Direction)

-- Convers a char to the corresponding direction
toDirection :: Char -> Direction
toDirection '^' = ( 0, -1)
toDirection '>' = ( 1,  0)
toDirection 'v' = ( 0,  1)
toDirection '<' = (-1,  0)
toDirection _   = error "Unhandled character"

-- Processes map data
processMap :: [[Char]] -> (Move, [Position])
processMap xss = (((x0, y0), direction), positions)
    where
        direction   = toDirection d
        positions   = map (\(x, y, _) -> (x, y)) obstacles
        obstacles   = filter (\(_, _, c) -> c == '#') xss'
        (x0, y0, d) = head $ filter (\(_, _, c) -> c `elem` "<>v^") xss'
        xss'        = [(x, y, (xss !! y) !! x) | x <- [0..length (head xss) - 1], y <- [0.. length xss - 1]]

-- Rotates a given direction clockwise
rotate :: Direction -> Direction
rotate ( 0, -1) = ( 1,  0)
rotate ( 1,  0) = ( 0,  1)
rotate ( 0,  1) = (-1,  0)
rotate (-1,  0) = ( 0, -1)
rotate _        = error "Unhandled rotation"

-- Move and slides until reaching out of bounds OR a loop
moveAndSlide :: Int -> Int -> Data.Set.Set Position -> Move -> Data.Set.Set Move -> Either [Move] [Move]
moveAndSlide xMax yMax os ((x0, y0), (u0, v0)) xss    | ((x0, y0), (u0, v0)) `Data.Set.member` xss       = Right $ Data.Set.elems xss                             -- Loop #TODO : need to speed this up
                                                        | x0 < 0 || x0 > xMax  || y0 < 0 || y0 > yMax    = Left  $ Data.Set.elems xss                             -- Out of bounds
                                                        | otherwise = case (x1, y1)`Data.Set.member` os of
                                                            True  -> moveAndSlide xMax yMax os ((x0, y0), (u1, v1)) xss                                           -- Rotate clockwise
                                                            False -> moveAndSlide xMax yMax os ((x1, y1), (u0, v0)) (Data.Set.insert ((x0, y0), (u0, v0)) xss)    -- Keep going
                                                        where    
                                                            ((x1, y1), (u1, v1)) = ((x0 + u0, y0 + v0), rotate (u0, v0))
                                            
-- Executes a single step
patrol :: Patrol -> Either [Move] [Move]
patrol ((p, d), os) = moveAndSlide xMax yMax (Data.Set.fromList os) (p, d) (Data.Set.empty) where
    xMax = maximum $ map fst os
    yMax = maximum $ map snd os

-- Finds a loop
findLoop ::  Patrol -> [Position] -> [Position] -> [Position]
findLoop  _            [] ls         = ls
findLoop ((p0, d0), os) (p1:ps) ls   = case patrol ((p0, d0), p1:os) of
                                            Left  _ -> findLoop ((p0, d0), os) ps ls 
                                            Right _ -> findLoop ((p0, d0), os) ps (p1:ls) 

-- Returns a list with unique elements
unique :: Eq a => [a] -> [a]
unique = reverse . nub . reverse

-- Reads the test input
readInputs :: IO Patrol
readInputs = do
    contents <- readFile inputFile
    pure $ processMap (lines contents)

-- The solver for part #1 of the puzzle
solvePart1 :: Patrol -> Int
solvePart1 p = length $ (group . sort) ps
    where
        ps = map fst (fromLeft [] ms)
        ms = patrol p

-- The solver for part #2 of the puzzle
solvePart2 :: Patrol -> Int
solvePart2 p = length $ ls
    where
        ls  = findLoop p ps' []
        ps' = unique (init ps)
        ps  = map fst (fromLeft [] ms)
        ms  = patrol p
        xs  = map fst ((\(p, os) -> os) p)
        ys  = map snd ((\(p, os) -> os) p)

-- The full solver
day6Solver :: IO [Int]
day6Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]