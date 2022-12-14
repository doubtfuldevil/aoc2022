{-# LANGUAGE TupleSections #-}
module AoC14 where

import Data.List ( sort )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import qualified Data.Set as Set

---------------------------------------
-- helper functions
---------------------------------------
getInput :: String -> String -> IO [String]
getInput mode day = do
    let filename = mode ++ day
    h <- openFile filename ReadMode
    content <- hGetContents h
    return (lines content)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = takeWhile (/=del) xs:splitOn del (safeTail $ dropWhile (/=del) xs)
---------------------------------------
-- main functionality
---------------------------------------
type Input = [String]
type Coord = (Int,Int)
type Maze = Set.Set Coord

sandStart :: Coord 
sandStart = (500,0)

maxY :: Set.Set Coord -> Int 
maxY =  Set.foldr' (\(x,y) z -> if y > z then y else z) 0

parseCoord :: String -> (Int,Int)
parseCoord xs = (read $ takeWhile (/=',') xs, read $ safeTail $ dropWhile (/=',') xs)

parseRockPath :: String -> [Coord]
parseRockPath = map parseCoord . filter (/= "->")  . splitOn ' '

buildRockPath :: Coord -> Coord -> [Coord]
buildRockPath (x1,y1) (x2,y2)
    | x1 < x2   = map (,y1) [x1..x2]
    | x2 < x1   = map (,y1) [x2..x1]
    | y1 < y2   = map (x1,) [y1..y2]
    | y2 < y1   = map (x1,) [y2..y1]
    | otherwise = error "not a straight line"

allPaths :: [Coord] -> [Coord]
allPaths [_] = []
allPaths (p1:p2:ps) = buildRockPath p1 p2 ++ allPaths (p2:ps)
allPaths _ = error "a path needs at least 2 points"

createMaze :: Input -> Maze 
createMaze =  Set.unions . map (Set.fromList .  allPaths  . parseRockPath )

oneGrain :: Int -> Coord -> Maze -> Maybe Coord 
oneGrain fl (x,y) m
    | y == fl                    = Nothing
    | Set.notMember (x,y+1) m    = oneGrain fl (x,y+1) m
    | Set.notMember (x-1,y+1) m  = oneGrain fl (x-1,y+1) m
    | Set.notMember (x+1,y+1) m  = oneGrain fl (x+1,y+1) m
    | otherwise                  = Just (x,y)

sandstorm :: Int -> Int -> Maze -> Int 
sandstorm fl c m = case oneGrain fl sandStart m of 
    Just coord  -> sandstorm fl (c+1) (Set.insert coord m)
    Nothing     -> c 

solution1 :: Input -> Int 
solution1 xs = let m = createMaze xs in sandstorm (maxY m) 0 m

-- part 2

floor' :: Maze -> Int 
floor' m = maxY m + 2

oneGrain2 :: Int -> Coord -> Maze -> Coord 
oneGrain2 fl (x,y) m
    | y+1 == fl                 = (x,y)
    | Set.notMember (x,y+1) m    = oneGrain2 fl (x,y+1) m
    | Set.notMember (x-1,y+1) m  = oneGrain2 fl (x-1,y+1) m
    | Set.notMember (x+1,y+1) m  = oneGrain2 fl (x+1,y+1) m
    | otherwise                  = (x,y)

sandstorm2 :: Int -> Int -> Maze -> Int 
sandstorm2 fl c m = case oneGrain2 fl sandStart m of 
    (500,0)   -> c+1
    coord     -> sandstorm2 fl (c+1) (Set.insert coord m)

solution2 :: Input -> Int 
solution2 xs = let m = createMaze xs in sandstorm2 (floor' m) 0 m 
---------------------------------------
-- start functions
---------------------------------------
day :: [Char]
day = "14"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (solution1 testInput)
    realInput <- getInput "input" day
    print (solution1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print (solution2 testInput)
    realInput <- getInput "input" day
    print (solution2 realInput)
