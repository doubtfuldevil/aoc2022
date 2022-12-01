module AoC01 where

import Data.List ( sort )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )

---------------------------------------
-- helper functions
---------------------------------------

hFetchLines :: Handle -> IO [String]
hFetchLines h = do
    content <- hGetContents h
    let contentLines = lines content
    return contentLines

getTestInput :: String -> IO [String]
getTestInput day = do
    let filename = "test" ++ day
    h <- openFile filename ReadMode
    hFetchLines h

getRealInput :: String -> IO [String]
getRealInput day = do
    let filename = "input" ++ day
    h <- openFile filename ReadMode
    hFetchLines h

---------------------------------------
-- main functionality
---------------------------------------

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = takeWhile (/=del) xs:splitOn del (safeTail $ dropWhile (/=del) xs)
    
elves :: [String] -> [[String]]
elves = splitOn ""

elfCalories :: [String] -> Int
elfCalories ss = sum $ map (\x -> read x :: Int) ss

caloriesList :: [[String]] -> [Int]
caloriesList = reverse . sort . map elfCalories

maxElf :: [String] -> Int
maxElf = head . caloriesList . elves

max3Elves :: [String] -> Int
max3Elves = sum . take 3 . caloriesList . elves

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "01"

part1 :: IO ()
part1 = do
    testInput <- getTestInput day
    print(maxElf testInput)
    realInput <- getRealInput day
    print(maxElf realInput)

part2 :: IO ()
part2 = do
    testInput <- getTestInput day
    print(max3Elves testInput)
    realInput <- getRealInput day
    print(max3Elves realInput)
