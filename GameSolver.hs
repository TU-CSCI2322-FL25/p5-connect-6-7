module GameSolver where

import Connect4
import Data.Maybe

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
