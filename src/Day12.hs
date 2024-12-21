module Day12 (day12Solver) where

import Data.List
import Data.Ord

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day12_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day12_input.txt"

-- The map type
type Map = [[Char]]

-- A singular plot
data Plot = Plot {
    plant     :: Char,
    coord     :: (Int, Int),
    area      :: Int,
    perimeter :: Int
} deriving (Eq, Ord)

instance Show Plot where
    show p = show $ plant p

-- Reads the test input
readInputs :: IO Map
readInputs = do
    contents <- readFile inputFile
    return $ lines contents

-- Builds a plot from a given map coordinate
mkPlot :: Map -> Int -> Int -> (Int, Int) -> Plot
mkPlot xss w h (x0, y0) = Plot { plant = pl, coord = (x0, y0), area = a, perimeter = p }
    where
        a           = 1
        p           = 4 - length adjacents
        adjacents   = [(x, y) | x <- [0..w - 1], y <- [0..h - 1], abs(x - x0) + abs(y - y0) == 1, ((xss !! y) !! x) == pl]
        pl          = ((xss !! y0) !! x0)

-- Makes plots from a map
mkPlots :: Map -> [[Plot]]
mkPlots xss = map mkPlotsRow [0..h - 1]
    where
        w            = length (head xss)
        h            = length xss
        mkPlotsRow y = [mkPlot xss w h (x, y) | x <- [0..w - 1]]

-- Flood fill (not the cleanest / most efficient implementation)
floodFillOne :: [[Plot]] -> (Int, Int) -> [Plot] -> [Plot]
floodFillOne xss (x, y) os  | o `elem` os                                       = []                                            -- Already added
                            | (not . null) os && (plant . head) os /= plant o   = []                                            -- Not compatible
                            | otherwise                                         = nub $ [o] ++ north ++ south ++ east ++ west   -- Add the rest
                            where
                                o     = ((xss !! y) !! x) 
                                w     = length (head xss)
                                h     = length xss
                                north = if y > (0    ) then floodFillOne xss (x, y - 1) (o:os) else []
                                south = if y < (h - 1) then floodFillOne xss (x, y + 1) (o:os) else []
                                east  = if x < (w - 1) then floodFillOne xss (x + 1, y) (o:os) else []
                                west  = if x > (0    ) then floodFillOne xss (x - 1, y) (o:os) else []

-- Flood fill (not the cleanest / most efficient implementation)
floodFillOne' :: [[Plot]] -> Int -> Int -> [Plot] -> [Plot]
floodFillOne' xss w h (p:ps)    | p `elem` ps                                       = []                                            -- Already added
                                | (not . null) ps && (plant . head) ps /= plant p   = []                                            -- Not compatible
                                | otherwise                                         = nub $ p : (north ++ south ++ east ++ west)    -- Add the rest
                                where
                                    -- Recurse
                                    north   = if (not . null) up    then floodFillOne' xss w h ((up++down++left++right)++(p:ps)) else []
                                    south   = if (not . null) down  then floodFillOne' xss w h ((down++up++left++right)++(p:ps)) else []
                                    east    = if (not . null) right then floodFillOne' xss w h ((right++up++down++left)++(p:ps)) else []
                                    west    = if (not . null) left  then floodFillOne' xss w h ((left++up++down++right)++(p:ps)) else []
                                    -- Neighbours
                                    (x, y)  = coord p
                                    up      = if y > (0    ) then filter (/= p) [(xss !! (y - 1)) !! (x + 0)] else []
                                    down    = if y < (h - 1) then filter (/= p) [(xss !! (y + 1)) !! (x + 0)] else []
                                    left    = if x < (w - 1) then filter (/= p) [(xss !! (y + 0)) !! (x - 1)] else []
                                    right   = if x > (0    ) then filter (/= p) [(xss !! (y + 0)) !! (x + 1)] else []

-- Flood fills multiple plots (caches previous regions to speed up the process somewhat)
floodFillMany :: [[Plot]] -> ([Plot],[Plot]) -> [[Plot]]
floodFillMany xss ([],      _) = []
floodFillMany xss ((p:ps), cs)  | p `elem` cs = floodFillMany xss (ps, cs)              -- Already a registered plot
                                | otherwise   = c : floodFillMany xss (ps, (c ++ cs))   -- Not registered ; flood fill & cache 
                                where 
                                    c = floodFillOne' xss w h [p]
                                    w = length (head xss)
                                    h = length xss

-- Finds all plots (not the cleanest / most efficient implementation)
findAllPlots :: [[Plot]] -> [[Plot]]
findAllPlots xss = floodFillMany xss (xss', [])
    where 
        xss'   = concat xss

-- Add plots together
addPlots :: [(Int, Int)] -> (Int, Int)
addPlots []            = (0, 0)
addPlots ((a0, p0):ps) = (a0 + a1, p0 + p1)
    where
        (a1, p1) = addPlots ps

-- Computes the price of a region
price :: (Int, Int) -> Int
price (a, p) = a * p

-- The solver for part #1 of the puzzle
solvePart1 :: Map -> Int
solvePart1 mss = sum $ map price regions''
    where
        regions'' = map addPlots regions'
        regions'  = map (map (\p -> (area p, perimeter p))) regions
        regions   = findAllPlots plots
        plots     = mkPlots mss

-- The solver for part #2 of the puzzle
solvePart2 :: Map -> Int
solvePart2 xss = 0

-- The full solver
day12Solver :: IO [Int]
day12Solver = do
    input <- readInputs
    let plots = mkPlots input
    let w     = length (head plots)
    let h     = length plots
    let p'    = plots !! 0 !! 6
    let p0    = floodFillOne plots (6, 0) []
    let p1    = floodFillOne' plots w h [p']
    putStrLn $ show p1
    return [0]--[solvePart1 input, solvePart2 input]
