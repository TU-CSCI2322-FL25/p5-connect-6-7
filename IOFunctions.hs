module IOFunctions where

import Connect4
import ConvertFormat
import GameSolver 

writeGame :: Game -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO (Maybe Game)
loadGame path = do
  contents <- readFile path
  let rContents = readGame contents
  return rContents

putBestMove :: Game -> IO ()
putBestMove game = case bestMove game of
  Nothing -> putStrLn "No winning or drawing move available, or the game is already over."
  Just m  -> do
    let nextGame = updateGame game m
    let outcome = whoWillWin nextGame
    putStrLn $ "Best move: column " ++ show m
    putStrLn $ "This move forces: " ++ (if outcome==Tie then "a tie" else show outcome ++ " to win")
