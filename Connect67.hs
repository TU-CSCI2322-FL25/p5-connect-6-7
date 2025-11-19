import Data.Maybe
import Data.List (transpose)

-- Story 1: Define data types or type aliases for a player, game state, move, and winner
data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

--Story 3
opponent :: Color -> Color
opponent Red    = Yellow
opponent Yellow = Red

updateGame :: Move -> Game -> Game
updateGame move game@(board, color)
    | m < 0 || m >= length board = game        -- invalid move
    | length col >= 6            = game        -- column full
    | otherwise =
        ( before ++ [col ++ [color]] ++ after  -- place piece bottom-up
        , opponent color                       -- next player
        )
  where
    m = fromInteger move
    (before, col:after) = splitAt m board

-- Story 2: Check the board for a winner
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

checkList :: [Maybe Color] -> Integer -> Winner -- take a list of colors, ex a column, and return just the color if there are 4 in a row or nothing.
checkList [x] counter = if counter >=4 then x else Nothing
checkList [] counter = Nothing
checkList (x:xs) counter
    | counter >= 4 = x
    | x == head xs = checkList xs (counter+1)
    | otherwise = checkList xs 1

checkWin :: Game -> Winner-- run functions to check all columns, rows, and diagonals.
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

-- Story 5 "Pretty-print a game into a string"
-- use "printStrLn &" in ghci to see the actual board with the \n as actual new lines
printGame :: Game -> String
printGame (board,turn) = unlines (("It's " ++ (show turn) ++ "\'s turn and the board is:"):(reverse colsToRows))
  where 
    colsToRows = transpose filled
    filled = [take 6 (ufc++"0000000")|ufc<-unfilled]
    unfilled = [[if color == Red then 'R' else 'Y'|color<-column]|column<-board]
 
testGame :: Game
testGame = (testBoard,Red)

testBoard :: Board
testBoard =

  [ [Red]
  , [Red,Yellow]
  , [Yellow]
  , []
  , [Red]
  , [Yellow]
  , []
  ]

-- Story 4 "Compute the legal moves from a game state, with a function of type Game -> [Move]."
legalMoves :: Game -> [Move]
legalMoves (board, turn) = [ind | (column, ind) <- assocBoard board, length column/=6]
assocBoard board = zip board [0,1..]
