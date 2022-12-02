{-# LANGUAGE InstanceSigs #-}
module AoC02 where

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
data Outcome = Win | Loss | Draw
    deriving Show
data Shape = Rock | Paper | Scissors
    deriving (Eq, Show)
type Round = (Shape, Shape)

instance Ord Shape where
    compare :: Shape -> Shape -> Ordering
    compare Rock Paper      = LT
    compare Rock Scissors   = GT
    compare Paper Rock      = GT
    compare Paper Scissors  = LT
    compare Scissors Rock   = LT
    compare Scissors Paper  = GT
    compare _ _             = EQ

charToShape :: Char -> Shape
charToShape 'A' = Rock
charToShape 'B' = Paper
charToShape 'C' = Scissors
charToShape 'X' = Rock
charToShape 'Y' = Paper
charToShape 'Z' = Scissors
charToShape _ = error "invalid Shape"

-- determine winner
play :: Round -> Outcome
play (x,y)
    | x == y    = Draw
    | y > x     = Win
    | otherwise = Loss

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

resultScore :: Outcome -> Int
resultScore Win = 6
resultScore Draw = 3
resultScore Loss = 0

transform :: String -> Round
transform [x,_,y] = (charToShape x, charToShape y)
transform _ = error "invalid round"

roundScore :: Round -> Int
roundScore r = shapeScore (snd r) + resultScore (play r)

totalScore1 :: [String] -> Int
totalScore1 = sum . map (roundScore . transform)

-- part 2

charToOutcome :: Char -> Outcome
charToOutcome 'X' = Loss
charToOutcome 'Y' = Draw
charToOutcome 'Z' = Win
charToOutcome _ = error "invalid outcome"

determineMove :: (Shape, Outcome) -> Shape 
determineMove (x, Draw) = x 
determineMove (x, Win) = head $ filter (> x) [Rock, Scissors, Paper]
determineMove (x, Loss) = head $ filter (< x) [Rock, Scissors, Paper]

transform2 :: String -> Round 
transform2 [x,_,y] = (a, determineMove (a, charToOutcome y))
    where a = charToShape x
transform2 _ = error "invalid round"

totalScore2 :: [String] -> Int
totalScore2 = sum . map (roundScore . transform2)

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "02"

part1 :: IO ()
part1 = do
    testInput <- getTestInput day
    print(totalScore1 testInput)
    realInput <- getRealInput day
    print(totalScore1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getTestInput day
    print(totalScore2 testInput)
    realInput <- getRealInput day
    print(totalScore2 realInput)
