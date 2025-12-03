module Main where

import System.Environment (getArgs)
import ConvertFormat
import IOFunctions

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    (p:_) -> return p
    _     -> do
      putStrLn "Enter game file to load:"
      getLine
  maybeGame <- loadGame path
  case maybeGame of 
    Nothing -> putStrLn "invalid file name"
    Just game -> 
      do  putStrLn "Loaded game:"
          putStrLn (printGame game)
          putBestMove game
