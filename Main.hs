module Main where

import System.Environment (getArgs)
import ConvertFormat
import IOFunctions

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