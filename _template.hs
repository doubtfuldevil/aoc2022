module AoC0X where

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

getInput :: String -> String -> IO [String]
getInput mode day = do
    let filename = mode ++ day
    h <- openFile filename ReadMode
    hFetchLines h

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
