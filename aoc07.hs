module AoC07 where

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

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn del xs = takeWhile (/=del) xs:splitOn del (safeTail $ dropWhile (/=del) xs)

---------------------------------------
-- main functionality
---------------------------------------

-- datatypes
data File = File String Int
    deriving Show
data Dir = Dir String [Dir] [File]
    deriving Show
type Command = String

filesystem :: Dir
filesystem = Dir "FS" [emptyDir "/"] []

-- filesystem operations

filesize :: File -> Int
filesize (File _ s) = s

dirname :: Dir -> String
dirname (Dir name _ _) = name

emptyDir :: String -> Dir
emptyDir s = Dir s [] []

allFiles :: Dir -> [File]
allFiles (Dir _ ds fs) = fs ++ concatMap allFiles ds

allDirs :: Dir -> [Dir]
allDirs (Dir s [] fs) = [Dir s [] fs]
allDirs (Dir s ds fs) = Dir s ds fs:concatMap allDirs ds

createSubdir :: Dir -> String -> Dir
createSubdir (Dir s ds fs) name = Dir s (emptyDir name:ds) fs

createSubfile :: Dir -> String -> Int -> Dir
createSubfile (Dir s ds fs) name size = Dir s ds (File name size:fs)

updateDirList :: [Dir] -> Dir -> Dir -> [Dir]
updateDirList ds old new = new:[d | d <- ds, dirname d /= dirname old]

getDir :: [Dir] -> String -> Dir
getDir ds name = head [d | d <- ds, dirname d == name]

-- parse input
parseInput :: Dir -> [Command] -> (Dir,[Command])
parseInput d [] = (d,[])
parseInput d (('$':' ':ss):cmds) =  parseCommand d (ss:cmds)
parseInput d (('d':'i':'r':' ':dir):cmds) = parseInput (createSubdir d dir) cmds
parseInput d (file:cmds) = parseInput (createSubfile d fname fsize) cmds
    where flist = splitOn ' ' file
          fname = flist !! 1
          fsize = read (head flist) :: Int


parseCommand :: Dir -> [Command] -> (Dir,[Command])
parseCommand (Dir s ds fs) (('c':'d':' ':dir):cmds)
    | dir == ".." = (Dir s ds fs,cmds)
    | otherwise = parseInput updatedDir updatedCmds
            where targetDir = getDir ds dir
                  (updatedTargetDir,updatedCmds) = parseInput targetDir cmds
                  updatedDir = Dir s (updateDirList ds targetDir updatedTargetDir) fs
parseCommand d ("ls":cmds) = parseInput d cmds
parseCommand _ _ = error "invalid command"

generateFS :: [String] -> Dir
generateFS input = fst $ parseInput filesystem input

getRoot :: Dir -> Dir
getRoot (Dir _ (d:ds) _) = d
getRoot _ = error "no root found"

-- get directory sizes

dirSize :: Dir -> Int
dirSize d = sum $ map filesize $ allFiles d

dirSizeList :: Dir -> [Int]
dirSizeList = map dirSize . allDirs . getRoot

solution1 :: [String] -> Int
solution1 xs = sum $ filter (<= 100000) (dirSizeList $ generateFS xs)

-- part2

totalDiskSpace :: Int
totalDiskSpace = 70000000

requiredSpace :: Int
requiredSpace = 30000000

unusedSpace :: Dir -> Int
unusedSpace d = totalDiskSpace - dirSize d

spaceToFree :: Dir -> Int
spaceToFree d = requiredSpace - unusedSpace d

solution2 :: [String] -> Int
solution2 xs = minimum $ filter (>= spaceToFree fs) (dirSizeList fs)
    where fs = generateFS xs

---------------------------------------
-- start functions
---------------------------------------

day :: [Char]
day = "07"

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
