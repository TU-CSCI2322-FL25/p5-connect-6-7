import Data.List (transpose)

data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

-- Story 5 "print the current connect 4 board"
printBoard :: Game -> String
printBoard = undefined