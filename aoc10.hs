module AoC10 where

import Data.List ( foldl' )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Maybe (fromJust)

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
type Cycle = Int
type State = (Cycle,Int)

noop :: State -> State
noop (c,x) = (c+1,x)

addX :: Int -> State -> State
addX n (c,x) = (c+2,x+n)

parse :: String -> (State -> State)
parse s
    | s == "noop"   = noop
    | otherwise     = addX (read (splitOn ' ' s !! 1) :: Int)

createRegister :: [State -> State] -> [State]
createRegister = reverse . foldl' (\(x:xs) f -> f x:x:xs) [(0,1)]

getX :: Cycle -> [State] -> Int
getX c reg = case lookup (c-1) reg of
  Nothing -> fromJust (lookup (c-2) reg)
  Just n -> n

cycleValues :: [Int]
cycleValues = take 6 $ iterate (+40) 20

register :: [String] -> [State]
register = createRegister . map parse

solution1 :: [String] -> Int
solution1 xs = sum [ c*x | (c,x) <- zip cycleValues (map (($ register xs) . getX) cycleValues) ]

-- part2

overlaps :: [State] -> Cycle -> Bool
overlaps reg c = pos == x || pos == x+1 || pos == x-1
    where pos = if c `mod` 40 == 0 then 39 else (c `mod` 40) -1
          x = getX c reg

pixel :: [State] -> Cycle -> Char
pixel reg c = if overlaps reg c then '#' else '.'

row :: [State] -> Cycle -> String
row reg start = map (pixel reg) (take 40 $ iterate (+1) start)

drawScreen :: [State] -> IO ()
drawScreen reg = do
    let rows = take 6 $ iterate (+40) 1
    mapM_ (putStrLn . row reg) rows
    putStrLn "////"

solution2 :: [String] -> IO ()
solution2 = drawScreen . register
---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "10"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (solution1 testInput)
    realInput <- getInput "input" day
    print(solution1 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print (register testInput)
    solution2 testInput
    realInput <- getInput "input" day
    solution2 realInput
