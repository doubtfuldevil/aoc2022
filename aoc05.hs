module AoC05 where

import Data.List ( sort, elemIndex )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Maybe (fromJust)

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

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = takeWhile (/=del) xs:splitOn del (safeTail $ dropWhile (/=del) xs)

---------------------------------------
-- main functionality
---------------------------------------

splitInput :: [String] -> ([String],[String])
splitInput l = (take (transition - 1) l, drop (transition + 1) l)
               where transition = fromJust $ elemIndex "" l

crateIndexes :: [Int]
crateIndexes = 1:iterate (+4) 5

parseStack :: [String] -> Int -> String
parseStack [] _ = []
parseStack (s:ss) i = s !! i : parseStack ss i

parseCrates :: [String]  -> [String]
parseCrates ss = go ss (take ((length . head) ss `div` 4 + 1) crateIndexes)
                 where go _ [] = []
                       go l (i:is) =  dropWhile (==' ') (parseStack l i) : go l is

parseMove :: String -> [Int]
parseMove m = map (\x -> read x :: Int) [msplit !! 1 , msplit !! 3, msplit !! 5 ]
             where msplit = splitOn ' ' m

parseMoves :: [String] -> [[Int]]
parseMoves = map parseMove

moveCrate :: Int -> Int -> [String] -> [String]
moveCrate from to cs = result
                       where (preFrom,wFrom) = splitAt (from-1) cs
                             e = (head . head) wFrom
                             fromStack = (tail . head) wFrom
                             postFrom = tail wFrom
                             elemRemoved = preFrom ++ [fromStack] ++ postFrom
                             (preTo,wTo) = splitAt (to-1) elemRemoved
                             toStack = e:head wTo
                             postTo = tail wTo
                             result = preTo ++ [toStack] ++ postTo

makeMove ::  [String] -> [Int] -> [String]
makeMove cs [0,_,_] = cs
makeMove cs [c,from,to] = makeMove (moveCrate from to cs) [c-1,from,to]
makeMove _ _ = error "illegal move"

makeMoves :: [[Int]] -> [String] -> [String]
makeMoves ms cs = foldl makeMove cs ms

rearrange :: [String] -> [String]
rearrange ss = makeMoves (parseMoves (snd input)) (parseCrates (fst input))
               where input = splitInput ss

-- part 2

moveCrates :: [String] -> [Int] -> [String]
moveCrates cs [count,from,to] = result
                where 
                    (preFrom,wFrom) = splitAt (from-1) cs
                    es = (take count . head) wFrom
                    fromStack = (drop count . head) wFrom
                    postFrom = tail wFrom
                    elemsRemoved = preFrom ++ [fromStack] ++ postFrom
                    (preTo,wTo) = splitAt (to-1) elemsRemoved
                    toStack = es ++ head wTo
                    postTo = tail wTo
                    result = preTo ++ [toStack] ++ postTo
moveCrates _ _ = error "illegal move"

makeMoves2 :: [[Int]] -> [String] -> [String]
makeMoves2 ms cs = foldl moveCrates cs ms

rearrange2 :: [String] -> [String]
rearrange2 ss = makeMoves2 (parseMoves (snd input)) (parseCrates (fst input))
               where input = splitInput ss

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "05"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print ((map head . rearrange) testInput)
    realInput <- getInput "input" day
    print((map head . rearrange) realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print ((map head . rearrange2) testInput)
    realInput <- getInput "input" day
    print((map head . rearrange2) realInput)
