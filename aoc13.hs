{-# LANGUAGE InstanceSigs #-}
module AoC13 where

import Data.List ( sort, elemIndex, elemIndices )
import Data.Char ( isDigit )
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
type Input = [String]
data Unit = I Integer | L List
    deriving Show
data List = Empty | List (Unit,List)
    deriving Show

instance Eq List where
    (==) :: List -> List -> Bool
    Empty == Empty               = True
    List (u1,l1) == List (u2,l2) = u1 == u2 && l1 == l2
    _ == _                       = False

instance Eq Unit where
    (==) :: Unit -> Unit -> Bool
    I x == I y  =  x == y
    L x == L y  =  x == y
    I i == L l  =  List (I i,Empty) == l
    L l == I i  =  List (I i,Empty) == l

instance Ord List where
    compare :: List -> List -> Ordering
    compare Empty Empty       = EQ
    compare Empty (List _)    = LT
    compare (List _) Empty    = GT
    compare (List (u1,l1)) (List (u2,l2))
        | u1 < u2   = LT
        | u1 > u2   = GT
        | otherwise = compare l1 l2

instance Ord Unit where
    compare :: Unit -> Unit -> Ordering
    compare (I x) (I y)       = compare x y
    compare (L x) (L y)       = compare x y
    compare (I i) (L l)       = compare (List (I i,Empty)) l
    compare (L l) (I i)       = compare l (List (I i,Empty))

isOrdered :: Ord a => (a,a) -> Bool
isOrdered (a,b) = a < b

parse :: String -> List
parse [] = Empty
parse ('[':']':xs) = List ( L Empty,parse xs)
parse ('[':xs) = let (a,b) = parseList xs in List (L (parse a), parse b)
parse (',':xs) = parse xs
parse xs = let (a,b) = parseNum xs in List (I a, parse b)

parseList :: String -> (String,String)
parseList xs  = go 0 (xs,[])
    where go 0 (']':xs,_) = ([],xs)
          go n (']':xs,_) = let (a,b) = go (n-1) (xs,[]) in (']':a,b)
          go n ('[':xs,_) = let (a,b) = go (n+1) (xs,[]) in ('[':a,b)
          go n (x:xs,_) = let (a,b) = go n (xs,[]) in (x:a,b)
          go _ _ = error "no closing bracket found"

parseNum :: String -> (Integer,String)
parseNum xs = (read $ takeWhile isDigit xs, safeTail $ dropWhile isDigit xs)

tuplify :: [a] -> (a,a)
tuplify (x:y:_) = (x,y)
tuplify _ = error "wrong number of elements in list"

solution1 :: Input -> Int
solution1 =  sum . map (+1) . elemIndices True . map (isOrdered . tuplify . map parse) . splitOn ""

-- part 2

dividerPackets :: [String]
dividerPackets = ["[[2]]","[[6]]"]

dividerIndices :: [List] -> Int
dividerIndices ls = foldr ((\x y -> (fromJust (elemIndex x ls) + 1)*y) . parse) 1 dividerPackets

solution2 :: Input -> Int
solution2 = dividerIndices . sort . map parse . (++) dividerPackets . filter (/= "")

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "13"

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
