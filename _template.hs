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




---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "0X"

part1 :: IO ()
part1 = do
    testInput <- getTestInput day
    print(testInput)
    --realInput <- getRealInput day
    --print(realInput)

part2 :: IO ()
part2 = do
    testInput <- getTestInput day
    print(testInput)
    --realInput <- getRealInput day
    --print(realInput)
