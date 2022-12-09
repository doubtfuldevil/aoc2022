module AoC09 where

import Data.List ( sort, nub )
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
type Pos = (Int,Int)
type Move = (Char,Int)

startPos :: (Int, Int)
startPos = (0,0)

parseMove :: String -> Move
parseMove (c:n) = (c,read n :: Int)
parseMove _ = error "invalid move"

touches :: Pos -> Pos -> Bool
touches (hx,hy) (tx,ty) = abs (hx-tx) <= 1 && abs (hy-ty) <= 1

adjustCoord :: Int -> Int -> Int
adjustCoord t h
    | h > t     = t+1
    | h < t     = t-1
    | otherwise = t

adjustTail :: Pos -> [Pos] -> [Pos]
adjustTail (hx,hy) ((tx,ty):ts)
    | touches (hx,hy) (tx,ty) = (tx,ty):ts
    | otherwise = adjustTail (hx,hy) ((adjustCoord tx hx, adjustCoord ty hy):(tx,ty):ts)
adjustTail _ _ = error "pos list may not be empty"

moveHead :: Move -> Pos -> Pos
moveHead ('R',n) (x,y) = (x+n,y)
moveHead ('L',n) (x,y) = (x-n,y)
moveHead ('U',n) (x,y) = (x,y+n)
moveHead ('D',n) (x,y) = (x,y-n)
moveHead _ _ = error "invald move"

allMoves :: (Pos,[Pos],[String]) ->  (Pos,[Pos],[String])
allMoves (h,ts,[]) = (h,ts,[])
allMoves (h,ts,m:ms) = allMoves (newH,newTS,ms)
    where newH = moveHead (parseMove m) h
          newTS = adjustTail newH ts

getSnd :: (a,b,c) -> b
getSnd (_,x,_) = x

positionsVisited :: [Pos] -> Int
positionsVisited = length . nub

solution1 :: [String] -> Int
solution1 xs = positionsVisited $ getSnd $ allMoves (startPos,[startPos],xs)

-- part2

initHeads :: [Pos]
initHeads = replicate 9 startPos

moveMiddle :: ([Pos],[Pos]) -> ([Pos],[Pos])
moveMiddle ([h],tailhist) = ([h],newHist)
    where newHist = adjustTail h tailhist
moveMiddle (h:m:hs,tailhist) = (h:movedRest,newHist)
    where movedM = head (adjustTail h [m])
          (movedRest,newHist) = moveMiddle (movedM:hs, tailhist)
moveMiddle _ = error "invalid rope"

oneMove :: ([Pos],[Pos]) -> Move ->  ([Pos],[Pos])
oneMove (hs,ts) (_,0) = (hs,ts)
oneMove (h:hs,ts) (c,n) = oneMove (newHS,newTS) (c,n-1)
    where newH = moveHead (c,1) h
          (newHS,newTS) = moveMiddle (newH:hs,ts)
oneMove _ _ = error "invalid movement"

allMoves2 :: ([Pos],[Pos],[String]) ->  ([Pos],[Pos],[String])
allMoves2 (hs,ts,[]) = (hs,ts,[])
allMoves2 (hs,ts,m:ms) = allMoves2 (newHS,newTS,ms)
    where (newHS,newTS) = oneMove (hs,ts) (parseMove m)

solution2 :: [String] -> Int
solution2 xs = positionsVisited $ getSnd $ allMoves2 (initHeads,[startPos],xs)
---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "09"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (solution1 testInput)
    realInput <- getInput "input" day
    print(solution1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "testb" day
    print (solution2 testInput)
    realInput <- getInput "input" day
    print(solution2 realInput)
