module AoC0X where

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




---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "0X"

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
