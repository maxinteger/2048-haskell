module Game2048 where

import System.Console.ANSI
import System.Random
import System.IO
import Control.Monad
import Data.List

type GameCell = Int
type GameRow = [GameCell]
type GameTable = [GameRow]
data GameData = GameData GameTable Int deriving (Show, Eq)

size = 4
initTable = replicate size (replicate size 0)


pullRow :: GameRow -> GameRow
pullRow []      = []
pullRow [x]     = [x]
pullRow (x:y:xs)
    | y == 0    = pullRow (x:xs) ++ [0]
    | x == 0    = pullRow (y:x:xs)
    | x == y    = pullRow (x+y:xs) ++ [0]
    | otherwise = x : pullRow (y:xs)


pullTable :: GameTable -> GameTable
pullTable xs = map pullRow xs


move :: Char -> GameTable -> GameTable
move 'w' t = transpose $ pullTable $ transpose t
move 'a' t = pullTable t
move 's' t = map reverse $ transpose (pullTable (transpose $ map reverse t))
move 'd' t = map reverse (pullTable (map reverse t))
move _   t = t

countEmptyCells :: GameTable -> Int
countEmptyCells tbl = length [y | x <- tbl, y <- x, y == 0]


checkEOG :: GameTable -> Bool
checkEOG tbl = countEmptyCells tbl > 0


checkWin :: GameTable -> Bool
checkWin tbl = countEmptyCells tbl > 0


pad :: Int -> String -> String
pad len str = take len (str ++ [' ',' ' ..])


getValidChar :: [Char] -> IO Char
getValidChar chars = do
    c <- hGetChar stdin
    if (c `elem` chars) then return c else getValidChar chars


flattan = foldl (++) []


breakAt :: Int -> GameRow -> GameTable
breakAt len row
    | (length row) <= len = [row]
    | otherwise           = [take len row] ++ breakAt len (drop len row)


setNewCell :: Int -> GameRow -> GameRow
setNewCell _ [] = []
setNewCell idx (r:rs)
    | idx == 0 && r == 0 = [2] ++ rs
    | otherwise          = r : setNewCell ((idx - 1) `max` 0) rs


addNextRndCell :: GameTable -> IO GameTable
addNextRndCell tbl = do
    rnd <- randomRIO (0, (countEmptyCells tbl) - 1)
    return (breakAt size (setNewCell rnd (flattan tbl) ))


game :: GameData -> IO()
game (GameData table score) = do
    --sequence (map (putStrLn . show) tbl)
    clearScreen
    putStrLn $ concat $  map (foldr (\x a -> (pad 5 (show x)) ++ a) "\n") table
    dir <- getValidChar ['a', 'w', 's', 'd']

    if (checkEOG table) then do
        movedTbl <- return (move dir table)
        newTbl <- if movedTbl /= table then addNextRndCell movedTbl else return table
        game $ (GameData newTbl score)
    else
        putStr "End"


main :: IO()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    iTbl <- addNextRndCell initTable
    game (GameData iTbl 0)
