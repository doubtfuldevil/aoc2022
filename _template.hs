module AoC1X where

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
type Input = [String]



---------------------------------------
-- start functions
---------------------------------------
day :: [Char]
day = "1X"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (testInput)
    --realInput <- getInput "input" day
    --print(realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print (testInput)
    --realInput <- getInput "input" day
    --print(realInput)