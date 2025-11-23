module TestCases where

import Connect4
import GameSolver
import IOFunctions
import ConvertFormat
import Data.Maybe

finishedGameRedWins :: Game
finishedGameRedWins =
  ( [ [Red, Red, Red, Red]
    , [Yellow, Yellow, Red, Red]
    , [Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red]
    , [Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red]
    , [Red, Yellow, Red, Yellow]
    ], Yellow)

finishedGameTie :: Game
finishedGameTie =
  ( [ [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    ], Red)

oneMoveFromEndRedWins :: Game
oneMoveFromEndRedWins =
  ( [ [Red, Yellow, Red, Red, Red]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    ], Red)

twoMovesFromEndRedWins :: Game
twoMovesFromEndRedWins =
  ( [ [Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    , [Yellow, Red, Red, Red, Yellow, Red]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    ], Yellow)

fourMovesFromEndTie :: Game
fourMovesFromEndTie =
  ( [ [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red]
    , [Yellow, Red, Yellow, Red]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Red, Yellow, Red, Yellow, Red, Yellow]
    , [Yellow, Red, Yellow, Red, Yellow, Red]
    ], Red)

-- Test functions using the new game states

testLegalMoves :: IO ()
testLegalMoves = do
  putStrLn "Legal moves for oneMoveFromEndRedWins:"
  print $ legalMoves oneMoveFromEndRedWins
  putStrLn "Legal moves for twoMovesFromEndRedWins:"
  print $ legalMoves twoMovesFromEndRedWins
  putStrLn "Legal moves for fourMovesFromEndTie:"
  print $ legalMoves fourMovesFromEndTie
  putStrLn "Legal moves for finishedGameRedWins:"
  print $ legalMoves finishedGameRedWins
  putStrLn "Legal moves for finishedGameTie:"
  print $ legalMoves finishedGameTie

testWhoWillWin :: IO ()
testWhoWillWin = do
  putStrLn "Who will win oneMoveFromEndRedWins:"
  print $ whoWillWin oneMoveFromEndRedWins
  putStrLn "Who will win twoMovesFromEndRedWins:"
  print $ whoWillWin twoMovesFromEndRedWins
  putStrLn "Who will win fourMovesFromEndTie:"
  print $ whoWillWin fourMovesFromEndTie
  putStrLn "Who will win finishedGameRedWins:"
  print $ whoWillWin finishedGameRedWins
  putStrLn "Who will win finishedGameTie:"
  print $ whoWillWin finishedGameTie

testBestMove :: IO ()
testBestMove = do
  putStrLn "Best move for oneMoveFromEndRedWins:"
  print $ bestMove oneMoveFromEndRedWins
  putStrLn "Best move for twoMovesFromEndRedWins:"
  print $ bestMove twoMovesFromEndRedWins
  putStrLn "Best move for fourMovesFromEndTie:"
  print $ bestMove fourMovesFromEndTie
  putStrLn "Best move for finishedGameRedWins:"
  print $ bestMove finishedGameRedWins
  putStrLn "Best move for finishedGameTie:"
  print $ bestMove finishedGameTie

-- Added a test for whoHasWon/checkWin based on your first file's requirements
testWhoHasWon :: IO ()
testWhoHasWon = do
  putStrLn "Who has won finishedGameRedWins:"
  print $ checkWin finishedGameRedWins
  putStrLn "Who has won finishedGameTie:"
  print $ checkWin finishedGameTie
  putStrLn "Who has won oneMoveFromEndRedWins:"
  print $ checkWin oneMoveFromEndRedWins

-- Added tests for makeMove and string conversion based on your first file's requirements

testMakeMoveAndConversion :: IO ()
testMakeMoveAndConversion = do
  let initialGame = (makeBoard, Red)
  let move = 3
  let gameAfterMove = updateGame initialGame move

  putStrLn "Game after Red moves to column 3:"
  putStrLn $ printGame gameAfterMove

  putStrLn "Game string representation (showGame):"
  let gameStr = showGame gameAfterMove
  putStrLn gameStr

  putStrLn "Game loaded from string (readGame):"
  let loadedGame = readGame gameStr
  putStrLn $ printGame loadedGame
  
  putStrLn "Verify updateGame is correct:"
  print $ gameAfterMove == loadedGame


runAllTests :: IO ()
runAllTests = do
  testLegalMoves
  testWhoHasWon
  testWhoWillWin
  testBestMove
  testMakeMoveAndConversion