module Board where

import Control.Monad
import Data.Array
import System.Random

data Status = Covered | Discovered | Flagged

data State = Mine | Neighbour Int

type Board = Array (Char, Int) (State, Status)

validate :: Board -> Bool
validate = undefined

genMines :: Char -> Int -> Int -> IO [(Char, Int)]
genMines ch i n = do
  gen <- getStdGen
  let chars = randomRs ('a', ch) (gen)
  gen' <- newStdGen
  let nums = randomRs (1, i) (gen')
  return (take n (zip chars nums))

genBoard :: Char -> Int -> Int -> IO Board
genBoard ch x n = do
  m <- genMines ch x n
  let initBoard = array (('a', 1), (ch, x)) [((i, j), (Neighbour 0, Covered)) | i <- ['a' .. ch], j <- [1 .. x]]
  return $ initBoard // [((i, j), (Mine, Covered)) | i <- ['a' .. ch], j <- [1 .. x], (i, j) `elem` m]
