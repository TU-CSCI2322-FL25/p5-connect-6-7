import data.Maybe

data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

type Winner = Maybe Color -- Player and color

type Move = Integer -- column number, color

type Game = (Board, Color)

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]



headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe

-- test

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