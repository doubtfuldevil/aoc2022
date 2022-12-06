module AoC01 where

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
    testInput <- getInput "test" day
    print(maxElf testInput)
    realInput <- getInput "input" day
    print(maxElf realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print(max3Elves testInput)
    realInput <- getInput "input" day
    print(max3Elves realInput)
