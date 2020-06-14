{-# LANGUAGE OverloadedStrings #-}

module Board where

import Control.Monad
import Data.Array
import Data.Char
import Data.Function ((&))
import Data.List
import Rainbow
import System.Console.ANSI
import System.Random

data Status = Covered | Discovered | Flagged deriving (Eq, Show)

data State = Mine | Neighbor Int deriving (Eq, Show)

data Action = Detonate | Flag | Unflag | Invalid deriving (Eq)

data Result = Win | Loss deriving (Eq)

type Board = Array (Char, Int) (State, Status)

gameLoop :: IO ()
gameLoop = do
  clearScreen
  putChunkLn $ "Welcome to minesweeper! Please choose difficulty:\n 1  9x9 - 10 mines\n 2  16x16 - 40 mines\n 3  26x16 - 99 mines" & fore blue
  variant <- getLine
  board <- startGame variant
  clearScreen
  putStrLn (showCoveredTable board)
  gameRes <- sessionLoop board
  if gameRes == Win
    then do
      putStrLn "YOU WON"
    else do
      putStrLn "YOU LOST"
  newGameQuestion

newGameQuestion :: IO ()
newGameQuestion = do
  putStrLn "Do you want to play again? (y/n)"
  answer <- getLine
  case answer of
    "y" -> gameLoop
    "n" -> do
      clearScreen
      return ()
    _ -> putStrLn "Invalid input. "

startGame :: String -> IO Board
startGame x
  | x == "1" = genBoard 'i' 9 10
  | x == "2" = genBoard 'p' 16 40
  | x == "3" = genBoard 'z' 16 99
  | x == "4" = genBoard 'd' 4 4

genBoard :: Char -> Int -> Int -> IO Board
genBoard ch x n = do
  m <- genMines ch x n
  let initBoard = array (('a', 1), (ch, x)) [((i, j), (Neighbor 0, Covered)) | i <- ['a' .. ch], j <- [1 .. x]]
  let p = evalBoard (initBoard // [((i, j), (Mine, Covered)) | i <- ['a' .. ch], j <- [1 .. x], (i, j) `elem` m])
  return p

genMines :: Char -> Int -> Int -> IO [(Char, Int)]
genMines ch i n = do
  chars <- generate ('a', ch)
  nums <- generate (1, i)
  return $ zip chars nums
  where
    generate r = replicateM n (randomRIO r)

showTable :: Board -> String
showTable arr = unlines $ map (unwords . map (show . unpack . fst . (arr !))) myIndices
  where
    myIndices = [[(x, y) | y <- [startY .. endY]] | x <- [startX .. endX]]
    ((startX, startY), (endX, endY)) = bounds arr

sessionLoop :: Board -> IO Result
sessionLoop board
  | didWeWin board = return Win
  | otherwise = do
    putStrLn "Choose a tile to discover (f.e. 'a 1')"
    tile <- getLine
    let inputAction = inputDirector board tile
    when (fst inputAction == Invalid) $ putStrLn "Input is invalid, try again"
    clearScreen
    if fst (board ! snd inputAction) == Mine && fst inputAction == Detonate
      then do
        putStrLn $ showCoveredTable (uncoverAllMines board)
        return Loss
      else do
        let newBoard = mkNewBoard board inputAction
        putStrLn (showCoveredTable newBoard)
        sessionLoop newBoard

uncoverAllMines :: Board -> Board
uncoverAllMines b = b // minesUncovered
  where
    minesList = filter (\(index, (state, status)) -> state == Mine) (assocs b)
    minesUncovered = map (\(ind, (state, status)) -> (ind, (state, Discovered))) minesList

mkNewBoard :: Board -> (Action, (Char, Int)) -> Board
mkNewBoard board action =
  case action of
    (Detonate, ind) ->
      if snd (board ! ind) == Flagged
        then board
        else uncover board ind
    (Flag, ind) -> addFlag board ind
    (Unflag, ind) -> removeFlag board ind
    (Invalid, _) -> board

addFlag :: Board -> (Char, Int) -> Board
addFlag b ind =
  let (state, status) = b ! ind
   in b // [(ind, (state, Flagged))]

removeFlag :: Board -> (Char, Int) -> Board
removeFlag b ind =
  let (state, status) = b ! ind
   in b // [(ind, (state, Covered))]

didWeWin :: Board -> Bool
didWeWin b =
  length coveredBoard <= (length coveredMineBoard)
  where
    coveredBoard = filter (\(state, status) -> status == Covered) (elems b)
    coveredMineBoard = filter (\(state, status) -> state == Mine) coveredBoard

unpack :: State -> String
unpack Mine = "*"
unpack (Neighbor x) = show x

showCoveredTable :: Board -> String
showCoveredTable arr =
  unlines
    . addAxis (endY - startY + 1)
    . map (intercalate "  " . map (coverDecider . (arr !)))
    $ indicesByRow
  where
    indicesByRow = [[(x, y) | y <- [startY .. endY]] | x <- [startX .. endX]]
    ((startX, startY), (endX, endY)) = bounds arr

addAxis :: Int -> [String] -> [String]
addAxis n b = ("  " ++ ints) : zippedBoard
  where
    zippedBoard = zipWith (++) chars b
    chars = map (: " ") ['a' ..]
    ints =
      unwords
        . take n
        . fmap (\n -> if n < 10 then show n ++ " " else show n)
        $ [1 ..]

coverDecider :: (State, Status) -> String
coverDecider (Neighbor 0, Discovered) = "·"
coverDecider (_, Flagged) = "F"
coverDecider (_, Covered) = "-"
coverDecider (x, Discovered) = unpack x

translate :: IO Board -> IO ()
translate b = b >>= putStrLn . showTable

inputDirector :: Board -> String -> (Action, (Char, Int))
inputDirector b s =
  case (words s) of
    [x, y] ->
      let ind = getCoords x y
       in (if isInBoard ind then Detonate else Invalid, ind)
    [ac, x, y] ->
      let ind = getCoords x y
       in (if isInBoard ind then flagger b ind else Invalid, ind)
    _ -> (Invalid, ('a', 1))
  where
    getCoords :: String -> String -> (Char, Int)
    getCoords x y = (head x, read y)
    isInBoard index = index `elem` indices b
    flagger :: Board -> (Char, Int) -> Action
    flagger b ind =
      if snd (b ! ind) == Flagged
        then Unflag
        else Flag

evalBoard :: Board -> Board
evalBoard b = listArray (bounds b) listCounts
  where
    helper Mine = const Mine
    helper (Neighbor _) = Neighbor . evalSpot b
    listCounts = map (\(i, (st, _)) -> (helper st i, Covered)) (assocs b)

evalSpot :: Board -> (Char, Int) -> Int
evalSpot b (ch, i) = length $ filter (== Mine) stateList
  where
    stateList = map (fst . (b !)) coords
    coords = findNeighbors b (ch, i)

uncover :: Board -> (Char, Int) -> Board
uncover b index = b // map (\(a, (b, _)) -> (a, (b, Discovered))) w
  where
    w = filter (\(a, b) -> a `elem` indices) (assocs b)
    indices = bfs b index

bfs :: Board -> (Char, Int) -> [(Char, Int)]
bfs b index =
  let visit [] visited = visited -- queue is empty, so we are done
      visit (ix : queue) visited =
        let nulNeighbors = filter (\a -> b ! a == (Neighbor 0, Covered)) ((findNeighbors b ix) \\ visited)
            visNeighbors = if b ! ix == (Neighbor 0, Covered) then findNeighbors b ix else nulNeighbors
         in visit (queue ++ nulNeighbors) (visNeighbors ++ visited)
   in visit [index] [index]

findNeighbors :: Board -> (Char, Int) -> [(Char, Int)]
findNeighbors b (ch, i) = filter (`elem` indices b) initList
  where
    initList =
      [ (succ ch, i),
        (pred ch, i),
        (succ ch, i + 1),
        (pred ch, i + 1),
        (succ ch, i - 1),
        (pred ch, i - 1),
        (ch, i + 1),
        (ch, i - 1)
      ]
