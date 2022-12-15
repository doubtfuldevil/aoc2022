{-# LANGUAGE TupleSections #-}
module AoC15 where

import Data.List ( sort )
import System.IO
    ( openFile, hGetContents, Handle, IOMode(ReadMode) )
import Data.Char (isDigit)
import qualified Data.Set as Set

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
type Coord = (Int,Int)
data Sensor = S Coord Coord Int
    deriving Show

pos :: Sensor -> Coord
pos (S p _ _) = p

beacon :: Sensor -> Coord
beacon (S _ b _) = b

mDist :: Sensor -> Int
mDist (S _ _ md) = md

parseNum :: String -> Int
parseNum = read . takeWhile (\x -> isDigit x || x == '-' ) . dropWhile (not . (\x -> isDigit x || x == '-' ))

parseSensor :: String -> Sensor
parseSensor s = S scoord bcoord md
    where nums = (map parseNum . filter (\x -> let h = head x in h == 'x' || h == 'y') . splitOn ' ') s
          scoord = (head nums,nums !! 1)
          bcoord = (nums !! 2,nums !! 3)
          md = manhattan scoord bcoord

manhattan :: Coord -> Coord -> Int
manhattan (sx,sy) (bx,by) = abs (sx - bx) + abs (sy - by)

coords :: Coord -> Int -> Int -> [Coord]
coords (x,y) xd yd = [(x+xd,y+yd),(x+xd,y-yd),(x-xd,y+yd),(x-xd,y-yd)]

sensorField :: Sensor -> [Coord]
sensorField s = go (pos s) (mDist s) 0
    where go c (-1) yd = []
          go c xd yd   = coords c xd yd ++ go c (xd-1) (yd+1)

getRow :: Int -> [Coord] -> [Coord]
getRow n = filter (\(x,y) -> y == n)

fillRow :: [Coord] -> Set.Set Coord
fillRow []  = Set.empty
fillRow [c] = Set.singleton c
fillRow ((x1,y1):(x2,y2):_) = Set.fromList $ map (,y1) [(min x1 x2)..(max x1 x2)]

solution1 :: Int -> Input -> Int
solution1 n xs = (length . flip Set.difference beacons . Set.unions . map fillRow . filter (not. null) . map (getRow n . sensorField )) sensors
    where sensors = map parseSensor xs
          beacons = Set.fromList $ map beacon sensors

-- part 2

inField :: Coord -> Sensor -> Bool 
inField c s = manhattan (pos s) c <= mDist s 

inAnyField :: Coord -> [Sensor] -> Bool 
inAnyField c = foldr (\x y -> inField c x || y)  False

outsideBorder :: Sensor -> [Coord]
outsideBorder s = go (pos s) ((mDist s)+1) 0
    where go c (-1) yd = []
          go c xd yd   = coords c xd yd ++ go c (xd-1) (yd+1)

inRange :: Int -> Coord -> Bool
inRange lim (x,y) = x >= 0 && x <= lim && y >=0 && y <= lim

tuningFreq :: Coord -> Int
tuningFreq (x,y) = x * 4000000 + y

solution2 :: Int -> Input -> Int
solution2 lim xs = (tuningFreq . head . filter (\x -> inRange lim x && (not . inAnyField x) ss) . concatMap outsideBorder) ss
    where ss = map parseSensor xs 
---------------------------------------
-- start functions
---------------------------------------
day :: [Char]
day = "15"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (solution1 10 testInput)
    realInput <- getInput "input" day
    print(solution1 2000000 realInput)

part2 :: IO ()
part2 = do
    testInput <- getInput "test" day
    print (solution2 20 testInput)
    realInput <- getInput "input" day
    print(solution2 4000000 realInput)