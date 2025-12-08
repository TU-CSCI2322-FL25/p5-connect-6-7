import Data.Maybe
import Data.List (transpose)
import System.Environment (getArgs)
import Data.List (maximumBy, minimumBy)

--file with all code not split up

-- Story 1: Define data types or type aliases for a player, game state, move, and winner
data Color = Red | Yellow deriving (Show,Eq) -- color

type Board = [[Color]] -- list of columns of colors

data Winner = Tie | Player Color deriving (Show,Eq)

type Move = Integer -- column number, color

type Game = (Board, Color)

type Rating = Integer

makeBoard :: Board -- make empty board 
makeBoard = [[],[],[],[],[],[],[]]

-- Story 2: Check the board for a winner
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

checkList :: [Maybe Color] -> Integer -> Maybe Color -- take a list of colors, ex a column, and return just the color if there are 4 in a row or nothing.
checkList [] _ = Nothing
checkList [x] counter = if counter >= 4 then x else Nothing
checkList (x:y:xs) counter
    | isNothing x  = checkList (y:xs) 1
    | x == y       = checkList (y:xs) (counter + 1)
    | counter >= 4 = x
    | otherwise    = checkList (y:xs) 1

checkWin :: Game -> Maybe Winner  -- run functions to check all columns, rows, and diagonals.
checkWin (board, color) = case safeHead $ catMaybes [checkVertical board, checkHorizontal board, checkPDiagonal board, checkNDiagonal board] of
                            Just x -> Just $ Player x
                            _ -> if null $ legalMoves (board, color) then Just Tie else Nothing

checkVertical :: Board -> Maybe Color
checkVertical board = let win = catMaybes [checkList (map Just column) 1 | column <- board]
                           in safeHead win

checkHorizontal :: Board -> Maybe Color
checkHorizontal board = safeHead $ catMaybes [checkList row 1 | row <- rows]
  where
    maxHeight = maximum (map length board)
    rows = [[if row < length col then Just (col !! row) else Nothing | col <- board] | row <- [0..maxHeight-1]]

checkPDiagonal :: Board -> Maybe Color
checkPDiagonal board = safeHead $ catMaybes [checkList [safeGet col (row + offset) | (col, offset) <- zip (drop startCol board) [0..]] 1| startCol <- [0..3], row <- [0..5]]
  where safeGet col idx = if idx < length col then Just (col !! idx) else Nothing

checkNDiagonal :: Board -> Maybe Color
checkNDiagonal board = safeHead $ catMaybes [checkList [safeGet col (row - offset) | (col, offset) <- zip (drop startCol board) [0..]] 1| startCol <- [0..3], row <- [3..5]]
  where safeGet col idx = if idx >= 0 && idx < length col then Just (col !! idx) else Nothing

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

--Story 10 finds a move that forces a win and returns it if it can
--if none do, returns a move that can force a tie, if none do that either or the game is over, returns nothing
bestMove :: Game -> Maybe Move
bestMove game@(board,color) = if isJust (checkWin game) || bestScore == -1 then Nothing else Just move
  where (bestScore,move) = maximum [(scoreOutcome color (whoWillWin (updateGame game move)),move)|move<-legalMoves game]

--Story 12 read the game from format in test1.csv
readGame :: String -> Game
readGame str = ([map toColor (words col)|col<-cols],toColor turn)
  where
    (turn:cols) = lines str
    toColor s = if s == "Red" then Red else Yellow

--Story 13 turn the game into the format in test1.csv
showGame :: Game -> String
showGame (board,turn) = show turn ++ "\n" ++ unlines [unwords (map toStr col)|col<-board]
  where toStr c = if c==Red then "Red" else "Yellow"

--Story 14 
writeGame :: Game -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO Game
loadGame path = do
  contents <- readFile path
  pure (readGame contents)

putBestMove :: Game -> IO ()
putBestMove game = case bestMove game of
  Nothing -> putStrLn "No winning or drawing move available, or the game is already over."
  Just m  -> do
    let nextGame = updateGame game m
    let outcome = whoWillWin nextGame
    putStrLn $ "Best move: column " ++ show m
    putStrLn $ "This move forces: " ++ (if outcome==Tie then "a tie" else show outcome ++ " to win")

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    (p:_) -> pure p
    _     -> do
      putStrLn "Enter game file to load:"
      getLine
  game <- loadGame path
  putStrLn "Loaded game:"
  putStrLn (printGame game)
  putBestMove game

--Story 17 return integer estimate based off game's state 
--evaluation should return a positive value for red and negative for yellow

rateGame :: Game -> Rating
rateGame g
  | checkWin g == Just (Player Red)    = bigWin   -- Red wins
  | checkWin g == Just (Player Yellow) = bigLoss  -- Yellow wins
  | otherwise                          = pieceDifference (fst g)
  where
    bigWin  =  1000000 :: Integer -- must be larger than a non endgame
    bigLoss = -1000000 :: Integer

--helper to get the difference in pieces on board
pieceDifference :: Board -> Rating
pieceDifference board = count Red board - count Yellow board
  where
    count c b = toInteger $ length [rowElem | column <- b, rowElem <- column, rowElem == c]

--story 18 cut of depth
whoWillWinDepth :: Game -> Int -> (Rating, Maybe Move)
whoWillWinDepth game depth = bestOutcome game depth
  where
    bestOutcome :: Game -> Int -> (Rating, Maybe Move)
    bestOutcome g@(board, currentColor) d
      | d == 0 = (rateGame g, Nothing)  -- Depth cutoff
      | otherwise = case checkWin g of
          Just Tie -> (0, Nothing)
          Just (Player winner) -> if winner == currentColor then (1, Nothing) else (-1, Nothing)
          Nothing ->
            if null moves
              then (0, Nothing)  -- No moves means tie
              else if currentColor == Red  -- Red tries to maximize, Yellow tries to minimize
                then maximumBy compareRating [(fst (bestOutcome (updateGame g move) (d - 1)), Just move) | move <- moves]
                else minimumBy compareRating [(fst (bestOutcome (updateGame g move) (d - 1)), Just move) | move <- moves]
      where
        moves = legalMoves g
        evalGame = rateGame g

compareRating :: (Rating, Maybe Move) -> (Rating, Maybe Move) -> Ordering
compareRating (rating1, _) (rating2, _) = compare rating1 rating2