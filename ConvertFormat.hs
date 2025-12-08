module ConvertFormat where

import Connect4
import Data.List (transpose)

-- Story 5 "Pretty-print a game into a string"
-- use "printStrLn &" in ghci to see the actual board with the \n as actual new lines
printGame :: Game -> String
printGame (board,turn) = unlines (("It's " ++ show turn ++ "\'s turn and the board is:"):reverse colsToRows)
  where
    colsToRows = transpose filled
    filled = [take 6 (ufc++"0000000")|ufc<-unfilled]
    unfilled = [[if color == Red then 'R' else 'Y'|color<-column]|column<-board]

--Story 12 read the game from format in test1.csv
readGame :: String -> Maybe Game
readGame str =
  case lines str of
    [] -> Nothing
    (turnStr:colStrs) -> 
        do turn <- toColor turnStr
           board <- sequence [sequence (map toColor (words col))|col<-colStrs]
           return (board, turn)
  where
    toColor s
      | s=="Red" = Just Red
      | s=="Yellow" = Just Yellow
      | otherwise = Nothing

--Story 13 turn the game into the format in test1.csv
showGame :: Game -> String
showGame (board,turn) = show turn ++ "\n" ++ unlines [unwords (map toStr col)|col<-board]
  where toStr c = if c==Red then "Red" else "Yellow"
