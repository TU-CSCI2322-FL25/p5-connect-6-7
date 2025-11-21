import Data.Maybe
import Data.List (transpose)


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
checkList [x] counter = if counter >=4 then x else Nothing
checkList [] counter = Nothing
checkList (x:xs) counter
    | counter >= 4 = x
    | x == head xs = checkList xs (counter+1)
    | otherwise = checkList xs 1

checkWin :: Game -> Maybe Winner  -- run functions to check all columns, rows, and diagonals.
checkWin (board, color) = case safeHead $ catMaybes [checkVertical board, checkHorizontal board, checkPDiagonal board, checkNDiagonal board] of
                            Just x -> Just $ Player x
                            _ -> if null $ legalMoves (board, color) then Just Tie else Nothing

checkVertical :: Board -> Maybe Color
checkVertical board = let win = catMaybes [checkList (map Just column) 1 | column <- board]
                           in safeHead win

checkHorizontal :: Board -> Maybe Color
checkHorizontal board = let win = catMaybes [checkList (map listToMaybe layer) 1 | layer <- [map (drop n) board | n <- [0..6]]]
                                 in safeHead win

checkPDiagonal :: Board -> Maybe Color
checkPDiagonal board = let win = catMaybes [checkList ([safeHead $ drop n column | (column, n) <- zip (drop p board) [i..]]) 1 | i <- [0..2], p <- [0..3]]
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


-- Story 5 "Pretty-print a game into a string"
-- use "printStrLn &" in ghci to see the actual board with the \n as actual new lines
printGame :: Game -> String
printGame (board,turn) = unlines (("It's " ++ show turn ++ "\'s turn and the board is:"):reverse colsToRows)
  where
    colsToRows = transpose filled
    filled = [take 6 (ufc++"0000000")|ufc<-unfilled]
    unfilled = [[if color == Red then 'R' else 'Y'|color<-column]|column<-board]

testGame :: Game
testGame = (testBoard,Yellow)

testBoard :: Board
testBoard =

  [ [Yellow,Yellow,Yellow]
  , [Red,Yellow,Red,Yellow]
  , []
  , [Yellow,Red,Yellow,Red,Yellow,Red]
  , [Red,Yellow,Red,Yellow,Red,Yellow]
  , [Red,Yellow,Red,Yellow,Red,Yellow]
  , [Yellow,Red,Yellow,Red,Yellow,Red]
  ]


--Story9 find winner
whoWillWin :: Game -> Winner
whoWillWin game@(board,color) = case checkWin game of
  Just x -> x
  Nothing -> if null moves then Tie
    else scoreToWinner color (maximum (map ((scoreOutcome color . whoWillWin) . updateGame game) moves))
  where moves = legalMoves game

scoreToWinner c s
  | s == 1  = Player c
  | s == 0  = Tie
  | s == -1 = if c == Red then Player Yellow else Player Red

scoreOutcome c (Player winner)
  | winner == c = 1
  | otherwise   = -1
scoreOutcome _ Tie = 0

--Story 10 best Move
bestMove :: Game -> Maybe Move
bestMove game@(board,color) = if isJust (checkWin game) then Nothing 
  else Just (snd (maximum [(scoreOutcome color (whoWillWin (updateGame game move)),move)|move<-legalMoves game]))
