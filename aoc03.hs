module AoC03 where

import Data.List ( sort )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Char (ord, isLower)
import Data.Maybe ( fromJust )

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

split :: [a] -> ([a],[a])
split l = splitAt (length l `div` 2) l

duplicate :: Eq a => ([a],[a]) -> Maybe a
duplicate ([],_) = Nothing
duplicate (x:xs,ys)
    | x `elem` ys   = Just x 
    | otherwise     = duplicate (xs,ys)

duplicates :: [String] -> [Char]
duplicates = map (fromJust . duplicate . split)

priority :: Char -> Int
priority c
    | isLower c = ord c - 96
    | otherwise = ord c - 38

prioSum :: [String] -> Int
prioSum l = sum $ map priority (duplicates l)

--- part 2

elfGroups :: Eq a => [a] -> [[a]]
elfGroups [] = []
elfGroups (x:y:z:xs) = [x,y,z]:elfGroups xs
elfGroups _ = error "invalid number of groups"

badge :: Eq a => [[a]] -> Maybe a
badge [[],_,_] = Nothing
badge [x:xs,ys,zs]
    | x `elem` ys && x `elem` zs  = Just x 
    | otherwise     = badge [xs,ys,zs]
badge y = error "invalid group "

badges :: [String] -> [Char]
badges l = map (fromJust . badge) (elfGroups l)

badgeSum :: [String] -> Int
badgeSum l = sum $ map priority (badges l)

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "03"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (prioSum testInput)
    realInput <- getInput "input" day
    print(prioSum realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print testInput
    print (badgeSum testInput)
    realInput <- getInput "input" day
    print (badgeSum realInput)
