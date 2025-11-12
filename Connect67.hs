{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use !!" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.Maybe

data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]



safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x



checkList :: [Maybe Color] -> Integer -> Winner
checkList [x] counter = if counter >=4 then x else Nothing
checkList [] counter = Nothing
checkList (x:xs) counter
    | counter >= 4 = x
    | x == head xs = checkList xs (counter+1)
    | otherwise = checkList xs 1

checkWin :: Game -> Winner
checkWin (board, color) = safeHead $ catMaybes [checkVertical board, checkHorizontal board, checkPDiagonal board, checkNDiagonal board]

checkVertical :: Board -> Winner
checkVertical board = let win = catMaybes [checkList (map Just column) 1 | column <- board]
                           in safeHead win

checkHorizontal :: Board -> Winner
checkHorizontal board = let win = catMaybes [checkList (map listToMaybe layer) 1 | layer <- [map (drop n) board | n <- [0..6]]] 
                                 in safeHead win

checkPDiagonal :: Board -> Winner
checkPDiagonal board = let win = catMaybes [checkList ([safeHead $ drop n column | (column, n) <- zip (drop p board) [i..]]) 1 | i <- [0..2], p <- [0..3]]
                               in safeHead win

checkNDiagonal :: Board -> Winner
checkNDiagonal board = checkPDiagonal (reverse board)