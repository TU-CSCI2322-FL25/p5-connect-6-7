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

updateBoardRows :: Board -> Move -> Color -> Board
updateBoardRows board move color
    | move < 0 || move >= fromIntegral (length board) = board 
    | length (board !! colIndex) >= 6 = board
    | otherwise = take colIndex board ++ [updatedColumn] ++ drop (colIndex + 1) board
    where
        colIndex = fromIntegral move
        column = board !! colIndex
        updatedColumn = color : column
        
-- Story 2: Check the board for a winner
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
