module GameSolver where

import Connect4
import Data.Maybe
import Data.List (maximumBy, minimumBy)

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