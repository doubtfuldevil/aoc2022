{-# LANGUAGE InstanceSigs #-}
module AoC11 where

import Data.List ( sort, find )
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

parseInt :: String -> Int
parseInt x = read x :: Int

updateList :: Eq a => [a] -> a -> a -> [a]
updateList l old new = takeWhile (/= old) l ++ [new] ++ safeTail (dropWhile (/= old) l)
---------------------------------------
-- main functionality
---------------------------------------
type Input = [String]
type ID = Int
type Item = Int
type Op = (Item -> Item)
type Test = (Item -> Bool)
type Dest = (ID,ID)
type Counter = Int
data Monkey = Monkey ID [Item] Op Test Dest Counter

instance Show Monkey where
    show :: Monkey -> String
    show (Monkey id items _ _ _ c) = "Monkey " ++ show id ++ ": " ++ show items ++ " | " ++ show c

instance Eq Monkey where
    (==) :: Monkey -> Monkey -> Bool
    (Monkey id1 _ _ _ _ _) == (Monkey id2 _ _ _ _ _) = id1 == id2

getID :: Monkey -> ID 
getID (Monkey id _ _ _ _ _) = id

getItems :: Monkey -> [Item]
getItems (Monkey _ items _ _ _ _) = items

mTest :: Monkey -> Test
mTest (Monkey _ _ _ test _ _) = test

getDest :: Monkey -> Dest
getDest (Monkey _ _ _ _ dest _) = dest

getCounter :: Monkey -> Counter
getCounter (Monkey _ _ _ _ _ c) = c

updateItems :: Monkey -> [Item] -> Monkey
updateItems (Monkey id _ o t d c) i = Monkey id i o t d c

updateCounter :: Monkey -> Int -> Monkey
updateCounter (Monkey id i o t d c) n = Monkey id i o t d (c+n)

newMonkey :: ID -> [Item] -> Op -> Test -> Dest -> Monkey
newMonkey id i o t d = Monkey id i o t d 0

-- parse Input

parseInput :: Input -> [Monkey]
parseInput [] = []
parseInput xs = parseMonkey (takeWhile (/= "") xs):parseInput (safeTail $ dropWhile (/= "") xs)

parseID :: String -> ID 
parseID = parseInt . init . last . splitOn ' '

parseItemList :: String -> [Item]
parseItemList s = map parseInt (splitOn ',' (splitOn ':' s !! 1))

parseFunc :: String -> (Int -> Int -> Int)
parseFunc "+" = (+)
parseFunc "*" = (*)
parseFunc _ = error "invalid fucntion string"

createOp :: [String] -> Op
createOp ["old",f,"old"] = \x -> (parseFunc f) x x
createOp ["old",f,n] = \x -> (parseFunc f) x (parseInt n)
createOp _ = error "invalid Op string"

parseOp :: String -> Op
parseOp = createOp . tail . dropWhile (/= "=") . splitOn ' '

parseTest :: String -> Test
parseTest s x = x `mod` (parseInt . last . splitOn ' ') s == 0

parseDest :: String -> String -> Dest 
parseDest a b = (f a, f b)
    where f = parseInt . last . splitOn ' '

parseMonkey :: [String] -> Monkey
parseMonkey xs = newMonkey id items op test dest
    where id = parseID (head xs)
          items = parseItemList (xs !! 1)
          op = parseOp (xs !! 2)
          test = parseTest (xs !! 3)
          dest = parseDest (xs !! 4) (xs !! 5)

-- monkey business

getMonkey :: ID -> [Monkey] -> Monkey 
getMonkey id = fromJust . find (\x -> getID x == id) 

inspect :: Monkey -> Op 
inspect (Monkey _ _ op _ _ _) = op

relief :: Item -> Item
relief i = i `div` 3

itemDest :: Monkey -> Item -> (Item, ID) 
itemDest m i =  (modItem, destID)
    where modItem = (relief . inspect m) i
          dest = getDest m
          destID = if mTest m modItem then fst dest else snd dest

throwItems :: [Monkey] -> [(Item, ID)] -> [Monkey]
throwItems ms [] = ms 
throwItems ms ((item,id):is) = throwItems (updateList ms oldM newM) is
    where oldM = getMonkey id ms 
          newItemList = getItems oldM ++ [item]
          newM = updateItems oldM newItemList

turn :: [Monkey] -> Monkey -> [Monkey]
turn ms m = updateList newMS m newM 
    where items = getItems m 
          newM = updateCounter (updateItems m []) (length items)
          procItems = map (itemDest m) items 
          newMS = throwItems ms procItems

oneRound :: [Monkey] -> [Monkey]
oneRound ms = go ms ms 
    where go fullMS [] = fullMS
          go fullMS (m:ms) = go (turn fullMS m) newMS
            where newFullMS =  turn fullMS m
                  newMS = (safeTail . dropWhile (/= m)) newFullMS

monkeyBusiness :: Int -> [Monkey] -> [Monkey]
monkeyBusiness n = last . take (n+1) . iterate oneRound

solution1 :: Input -> Int 
solution1 = product . take 2 .reverse . sort . map getCounter . monkeyBusiness 20 . parseInput

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "11"

part1 :: IO ()
part1 = do
    testInput <- getInput "test" day
    print (solution1 testInput)
    realInput <- getInput "input" day
    print(solution1 realInput)
