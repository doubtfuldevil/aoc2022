module AoC08 where

import Data.List ( sort )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Char (digitToInt)

---------------------------------------
-- helper functions
---------------------------------------
getInput :: String -> String -> IO [String]
getInput mode day = do
    let filename = mode ++ day
    h <- openFile filename ReadMode
    content <- hGetContents h
    return (lines content)

---------------------------------------
-- main functionality
---------------------------------------
type Row = [Int]
type Grid = [Row]
type EvalGrid = [[Bool]]

generateGrid :: [String] -> Grid
generateGrid = map $ map digitToInt

visibleFromLeft :: Row -> Row
visibleFromLeft = go (-1)
    where go maxh [] = []
          go maxh [x] = if x > maxh then [-1] else [x]
          go maxh (x:xs)
            | x > maxh    = (-1):modRow
            | otherwise   = x:modRow
            where modRow = go (max maxh x) xs

visibleFromRight :: Row -> Row
visibleFromRight = reverse . visibleFromLeft . reverse

visibleFromSides :: Grid -> EvalGrid
visibleFromSides g = evalGrids (map visibleFromRight g) (map visibleFromLeft g)

evalGrids :: Grid -> Grid -> EvalGrid
evalGrids [] [] = []
evalGrids (x:xs) (y:ys) = [c==(-1) || d==(-1) | (c,d) <- zip x y] : evalGrids xs ys
evalGrids _ _ = error "invalid grids"

turnGrid :: [[a]] -> [[a]]
turnGrid [] = []
turnGrid ([]:_) = []
turnGrid xs = map head xs : turnGrid (map tail xs)

visibleFromTB :: Grid -> EvalGrid
visibleFromTB = turnGrid . visibleFromSides  . turnGrid

mergeEvalGrids :: EvalGrid -> EvalGrid -> EvalGrid
mergeEvalGrids [] [] = []
mergeEvalGrids (x:xs) (y:ys) = [c || d | (c,d) <- zip x y] :mergeEvalGrids xs ys
mergeEvalGrids _ _ = error "invalid eval grids"

countVisibleTrees :: Grid -> Int
countVisibleTrees g = sum $ map (length . filter (==True)) $ mergeEvalGrids (visibleFromSides g) (visibleFromTB g)

solution1 :: [String] -> Int
solution1 = countVisibleTrees . generateGrid

-- part 2

viewingTuple :: Row -> [(Int,Row,Row)]
viewingTuple r = map (\(i,x) -> (r !! i, (reverse . take i) r, drop (i+1) r)) zipped
    where zipped = zip [0..] r

evalSight :: Row -> Int -> Int
evalSight r h =  min (length (takeWhile (<h) r) + 1) (length r)

tupleSight :: (Int,Row,Row) -> Int
tupleSight (h,left,right) = evalSight left h * evalSight right h

viewToSides :: Grid -> Grid
viewToSides = map (map tupleSight . viewingTuple)

viewToTB :: Grid -> Grid
viewToTB = turnGrid . viewToSides . turnGrid

mergeViews :: Grid -> Grid -> Grid
mergeViews [] [] = []
mergeViews (x:xs) (y:ys) = [h*v | (h,v) <- zip x y] : mergeViews xs ys
mergeViews _ _ = error "invalid views"

scenicScore :: Grid -> Grid
scenicScore g = mergeViews (viewToSides g) (viewToTB g)

solution2 :: [String] -> Int
solution2 = maximum . map maximum . scenicScore . generateGrid
---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "08"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (solution1 testInput)
    realInput <- getInput "input" day
    print(solution1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print (solution2 testInput)
    realInput <- getInput "input" day
    print(solution2 realInput)
