data Turn = Player Color deriving (Show,Eq) -- plaayer

data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Turn -- Player and color

type Move = (Integer, Turn) -- column number, color

makeBoard :: Board -- make empty board
makeBoard = [[],[],[],[],[],[],[]]