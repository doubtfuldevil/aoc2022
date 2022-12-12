{-# LANGUAGE TupleSections #-}
module AoC12 where

import Data.List ( findIndex, elemIndex, elemIndices )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Char (ord)
import Data.Maybe (fromJust, isJust, mapMaybe)

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
type Input = [String]
type Coord = (Int,Int)
type Vertex = (Coord,Int)
type Queue = [Vertex]
type History = [Vertex]

findChar :: Input -> Char -> Coord
findChar i c = (x,y)
    where rows = map (elemIndex c) i
          y = (fromJust . findIndex isJust) rows
          x = fromJust (rows !! y)

start :: Input -> Coord
start i = findChar i 'S'

goal :: Input -> Coord
goal i = findChar i 'E'

maybeVal :: Input -> Coord -> Maybe Char
maybeVal i (x,y)
    | (x,y) == start i                             = Just 'a'
    | (x,y) == goal i                              = Just 'z'
    | x >= 0 && x <= maxX && y >= 0 && y <= maxY = Just ((i !! y) !! x)
    | otherwise                                  = Nothing
    where maxX = length (head i) - 1
          maxY = length i - 1

validNeighbour :: Input -> Coord -> Coord -> Bool
validNeighbour i c d = case maybeVal i d of
  Nothing -> False
  Just dval -> curr >= ord dval || ord dval - curr == 1
  where curr = (ord . fromJust . maybeVal i) c

edges :: Input -> Coord -> [Coord]
edges i (x,y) = left ++ right ++ up ++ down
    where left  = [(x-1,y) | validNeighbour i (x,y) (x-1,y)]
          right = [(x+1,y) | validNeighbour i (x,y) (x+1,y)]
          up    = [(x,y+1) | validNeighbour i (x,y) (x,y+1)]
          down  = [(x,y-1) | validNeighbour i (x,y) (x,y-1)]

notQueued :: [Vertex] -> Coord -> Bool
notQueued vs c = case lookup c vs of
    Nothing -> True
    Just _  -> False

bfs :: History -> Queue -> Input -> History
bfs hist [] i     = hist
bfs hist (q:qs) i = bfs newHist newQ i
    where newHist = q:hist
          steps = snd q
          newQ = qs ++ [(x,steps+1) | x <- edges i (fst q), notQueued hist x, notQueued qs x]

solution1 :: Input -> Int
solution1 i = (fromJust . lookup (goal i) . bfs [] [(start i,0)]) i

-- part 2

startList :: Input -> [Coord]
startList i = start i : concatMap (\(y,l) -> map (,y) l ) z
    where rows = map (elemIndices 'a') i
          z = zip [0..] rows

solution2 :: Input -> Int
solution2 i = (fromJust . lookup (goal i) . bfs [] (map (,0) (startList i))) i

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "12"

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
