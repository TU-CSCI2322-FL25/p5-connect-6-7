import Data.List (transpose)
data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

-- Story 5 "print the current connect 4 game"
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