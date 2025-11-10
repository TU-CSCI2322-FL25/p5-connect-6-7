data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Maybe Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

-- Story 5 "print the current connect 4 board"

showRow :: [Maybe Color] -> String
showRow row = unwords (map cellToString row)

cellToString :: Maybe Color -> String
cellToString (Just Red) = "R"
cellToString (Just Yellow) = "Y"
cellToString Nothing = "0"

printBoard :: Board -> String
printBoard board = unlines (map showRow board)


testBoard :: Board
testBoard =

  [ [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Just Red, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Just Yellow, Just Red, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just Red]
  ]