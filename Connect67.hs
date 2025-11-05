--data Tile = Space Color | Empty deriving (Show,Eq)
data Turn = Player Color deriving (Show,Eq)

data Color = Red | Yellow deriving (Show,Eq)

type Board = [[Color]]




makeBoard :: Board
makeBoard = [[],[],[],[],[],[],[]]