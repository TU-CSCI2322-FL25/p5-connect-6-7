module Connect4 where

import Data.Maybe

-- Story 1: Define data types or type aliases for a player, game state, move, and winner
data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

data Winner = Tie | Player Color deriving (Show,Eq)

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

-- Story 2: Check the board for a winner
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

checkList :: [Maybe Color] -> Integer -> Maybe Color -- take a list of colors, ex a column, and return just the color if there are 4 in a row or nothing.
checkList [] _ = Nothing
checkList [x] counter = if counter >= 4 then x else Nothing
checkList (x:y:xs) counter
    | isNothing x  = checkList (y:xs) 1
    | x == y       = checkList (y:xs) (counter + 1)
    | counter >= 4 = x
    | otherwise    = checkList (y:xs) 1

checkWin :: Game -> Maybe Winner  -- run functions to check all columns, rows, and diagonals.
checkWin (board, color) = case safeHead $ catMaybes [checkVertical board, checkHorizontal board, checkPDiagonal board, checkNDiagonal board] of
                            Just x -> Just $ Player x
                            _ -> if null $ legalMoves (board, color) then Just Tie else Nothing

checkVertical :: Board -> Maybe Color
checkVertical board = let win = catMaybes [checkList (map Just column) 1 | column <- board]
                           in safeHead win

checkHorizontal :: Board -> Maybe Color
checkHorizontal board = safeHead $ catMaybes [checkList row 1 | row <- rows]
  where
    maxHeight = maximum (map length board)
    rows = [[if row < length col then Just (col !! row) else Nothing | col <- board] | row <- [0..maxHeight-1]]

checkPDiagonal :: Board -> Maybe Color
checkPDiagonal board = safeHead $ catMaybes [checkList [safeGet col (row + offset) | (col, offset) <- zip (drop startCol board) [0..]] 1| startCol <- [0..3], row <- [0..5]]
  where safeGet col idx = if idx < length col then Just (col !! idx) else Nothing

checkNDiagonal :: Board -> Maybe Color
checkNDiagonal board = safeHead $ catMaybes [checkList [safeGet col (row - offset) | (col, offset) <- zip (drop startCol board) [0..]] 1| startCol <- [0..3], row <- [3..5]]
  where safeGet col idx = if idx >= 0 && idx < length col then Just (col !! idx) else Nothing

--Story 3

updateGame :: Game -> Move -> Game
updateGame game@(board,color) move = (updateBoard game move 0,if color==Red then Yellow else Red)
updateBoard :: Game -> Move -> Move -> Board
updateBoard (x:xs,color) move c = if move==c then (x++[color]):xs else x:updateBoard (xs,color) move (c+1)

-- Story 4 "Compute the legal moves from a game state, with a function of type Game -> [Move]."
legalMoves :: Game -> [Move]
legalMoves (board, turn) = [ind | (column, ind) <- assocBoard board, length column/=6]
assocBoard board = zip board [0,1..]