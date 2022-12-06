module AoC04 where

import Data.List ( sort )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )

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

pairs :: [String] -> [[String]]
pairs = map (splitOn ',')

parseSections :: [String] -> (Int,Int)
parseSections s = (read (head s) :: Int, read (s !! 1) :: Int)

sectionPairs :: [String] -> [(Int,Int)]
sectionPairs = map (parseSections . splitOn '-')

pairsList :: [String] -> [[(Int,Int)]]
pairsList l = map sectionPairs (pairs l)

contains :: [(Int,Int)] -> Bool
contains [x,y] = (fst x <= fst y && snd x >= snd y) || (fst y <= fst x && snd y >= snd x)
contains _ = error "invalid section"

countContains :: [String] -> Int
countContains l = length $ filter contains (pairsList l)

-- part 2

overlaps :: [(Int,Int)] -> Bool
overlaps [x,y] = (fst y <= snd x && fst y >= fst x)
            || (snd y <= snd x && snd y >= fst x)
            || (fst x <= snd y && fst x >= fst y)
            || (snd x <= snd y && snd x >= fst y)
overlaps _ = error "invalid section"

countOverlaps :: [String] -> Int
countOverlaps l = length $ filter overlaps (pairsList l)

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "04"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (countContains testInput)
    realInput <- getInput "input" day
    print(countContains realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print (countOverlaps testInput)
    realInput <- getInput "input" day
    print(countOverlaps realInput)
