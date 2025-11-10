import Data.List (transpose)

-- Story 1: Define data types or type aliases for a player, game state, move, and winner
data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

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

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe

-- Story 2 WIP

checkWin :: Game -> Winner
checkWin (board, color) = undefined

checkVertical :: Game -> Winner
checkVertical (board, _) = let win = catMaybes [aux column 0 | column <- board]
                           in if null win then Nothing else head win
                                where aux [] _ = Nothing
                                      aux [x] counter = if counter >=4 then Just x else Nothing
                                      aux (x:xs) counter = if counter >= 4 then Just x 
                                                           else if x == head xs then aux xs (counter+1) else aux xs 0

checkHorizontal :: Game -> Winner
checkHorizontal (board, color) = undefined



checkDiagonal :: Game -> Winner
checkDiagonal = undefined


-- Story 4 "Compute the legal moves from a game state, with a function of type Game -> [Move]."
legalMoves :: Game -> [Move]
legalMoves (board, turn) = [ind | (column, ind) <- assocBoard board, length column/=6]
assocBoard board = zip board [0,1..]
