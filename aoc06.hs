module AoC06 where

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
distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = notElem x xs && distinct xs

distinctN :: Eq a => [a] -> Int -> Int
distinctN xs num = go xs num
    where  go xs n
                | distinct (take num xs) = n
                | otherwise = go (tail xs) (n+1)

packetStart :: String -> Int
packetStart xs = distinctN xs 4

solution1 :: [String] -> Int
solution1 = packetStart . head 

-- part2

messageStart :: String -> Int
messageStart xs = distinctN xs 14

solution2 :: [String] -> Int
solution2 = messageStart . head

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "06"

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
