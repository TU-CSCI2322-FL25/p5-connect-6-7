module Main where

import System.Environment (getArgs)
import ConvertFormat
import IOFunctions
import System.Console.GetOpt

main :: IO ()
main = do
  args <- getArgs
  let (flags, nonOpts, _) = getOpt RequireOrder options args

  -- Get game file path
  path <- case nonOpts of
    (p:_) -> return p
    _     -> do
      putStrLn "Enter game file to load:"
      getLine

  maybeGame <- loadGame path
  case maybeGame of
    Nothing -> putStrLn "invalid file name"
    Just game -> do
      putStrLn "Loaded game:"
      putStrLn (printGame game)
      -- If no flags then default to best move
      let flagsToRun = if null flags then [flagBest] else flags
      mapM_ (runFlag game) flagsToRun

data Flag
  = flagLegal
  | flagWinner
  | flagWhoWillWin
  | flagBest
  | flagRate
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option [] ["legal"]       (NoArg flagLegal)      "Show legal moves"
  , Option [] ["winner"]      (NoArg flagWinner)     "Show winner of the finished game"
  , Option [] ["whowillwin"]  (NoArg flagWhoWillWin) "Predict winner from current state"
  , Option [] ["best"]        (NoArg flagBest)       "Show the best move"
  , Option [] ["rate"]        (NoArg flagRate)       "Rate the game"
  ]

runFlag :: Game -> Flag -> IO ()
runFlag game flag =
  case flag of
    flagLegal      -> do
      putStrLn "Legal moves:"
      print $ legalMoves game

    flagWinner     -> do
      putStrLn "Winner (if any):"
      print $ checkWin game

    flagWhoWillWin -> do
      putStrLn "Predicted winner:"
      print $ whoWillWin game

    flagBest       -> do
      putStrLn "Best move:"
      print $ bestMove game

    flagRate       -> do
      putStrLn "Game rating:"
      print $ rateGame game
