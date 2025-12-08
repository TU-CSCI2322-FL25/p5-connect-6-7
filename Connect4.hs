module Connect4 where

import Data.Maybe

-- Story 1: Define data types or type aliases for a player, game state, move, and winner
data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

data Winner = Tie | Player Color deriving (Show,Eq)

type Move = Integer -- column number, color

type Game = (Board, Color)

type Rating = Integer

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

-- Story 2: Check the board for a winner
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x



checkList :: [Maybe Color] -> Maybe Color -- taken a list, return if there are four colors in a row.
checkList [] = Nothing
checkList [_,_,_] = Nothing
checkList (Just Red:Just Red:Just Red:Just Red:xs) = Just Red
checkList (Just Yellow:Just Yellow:Just Yellow:Just Yellow:xs) = Just Yellow
checkList (_:xs) = checkList xs



checkWin :: Game -> Maybe Winner  -- run functions to check all columns, rows, and diagonals.
checkWin (board, color) = case safeHead $ catMaybes [checkVertical board, checkHorizontal board, checkPDiagonal board, checkNDiagonal board] of
                            Just x -> Just $ Player x
                            _ -> if null $ legalMoves (board, color) then Just Tie else Nothing 

checkVertical :: Board -> Maybe Color
checkVertical board = let win = catMaybes [checkList (map Just column) | column <- board]
                           in safeHead win

checkHorizontal :: Board -> Maybe Color
checkHorizontal board = let win = catMaybes [checkList (map listToMaybe layer) | layer <- [map (drop n) board | n <- [0..6]]] 
                                 in safeHead win

checkPDiagonal :: Board -> Maybe Color
checkPDiagonal board = let win = catMaybes [checkList ([safeHead $ drop n column | (column, n) <- zip (drop p board) [i..]]) | i <- [0..2], p <- [0..3]]
                               in safeHead win

checkNDiagonal :: Board -> Maybe Color
checkNDiagonal board = checkPDiagonal (reverse board)



--Story 3

updateGame :: Game -> Move -> Game
updateGame game@(board,color) move = (updateBoard game move 0,if color==Red then Yellow else Red)
updateBoard :: Game -> Move -> Move -> Board
updateBoard (x:xs,color) move c = if move==c then (x++[color]):xs else x:updateBoard (xs,color) move (c+1)

-- Story 4 "Compute the legal moves from a game state, with a function of type Game -> [Move]."
legalMoves :: Game -> [Move]
legalMoves (board, turn) = [ind | (column, ind) <- assocBoard board, length column/=6]
assocBoard board = zip board [0,1..]
