module Main where

import Board

main :: IO ()
main = do
  putStrLn "Welcome to minesweeper! Please choose difficulty:\n  9x9 - 5 mines\n 2  16x16 - 20 mines\n 3  26x16 - 99 mines"
  variant <- getLine
