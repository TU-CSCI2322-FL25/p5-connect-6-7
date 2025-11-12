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

updateBoardRows :: Board -> Color -> Board
updateBoardRows b move color
    | move < 0 || move >= fromIntegral (length board) = board 
    | length (board !! colIndex) >= 6 = board
    | otherwise = take colIndex board ++ [updatedColumn] ++ drop (colIndex + 1) board
    where
        colIndex = fromIntegral move
        column = board !! colIndex
        updatedColumn = color : column


