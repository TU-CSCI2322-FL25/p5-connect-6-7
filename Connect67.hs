data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

--Story 3

rowsEmpty :: Board -> Bool
rowsEmpty = all null

checkBoardRows :: Board -> Board
checkBoardRows b
    | rowsEmpty b =
    | otherwise   = b

-- updateBoard :: Game -> Move -> Game
-- updateBoard (board, color) = Move color
