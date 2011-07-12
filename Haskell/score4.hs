module Main where

import Data.Map as M (Map, findWithDefault, insert, insertWith', keys, empty, map, member, foldrWithKey, lookup, toList)
import Data.List (foldl',find)
import Data.Char (digitToInt)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Prelude as P
import Data.Array.IArray (accumArray, (!))
import Data.Array.Unboxed (UArray)
--import Debug.Trace (trace)

(width,height) = (7,6)
maxDepth = 7
orangeWins = 1000000
yellowWins = -orangeWins

data Cell = Orange | Yellow
type Board = Map (Int,Int) Cell
type Score = Int

otherColor Orange = Yellow
otherColor Yellow = Orange

dropDisk :: Board -> Int -> Cell -> Board
dropDisk board column color | 0 < row   = insert (row-1, column) color board
                            | otherwise = error "full board"
  where row = fst . minimum . ((height,column) :) . filter ((== column) . snd) . keys $ board

scoreBoard :: Board -> Int
scoreBoard board | 0 `member` counts = yellowWins
                 | 8 `member` counts = orangeWins
                 | otherwise         = M.foldrWithKey (\i c s -> s + (coeff i) * c) 0 counts
  where counts = horSpans . verSpans . incDiags . decDiags $ empty
        scores :: UArray (Int,Int) Int
        scores = accumArray (\_ y -> rateCell y) 0 ((0,0),(height-1,width-1)) (toList board)
        rateCell cell = {-# SCC "rateCell" #-} case cell of
            Orange -> 1
            Yellow -> -1
        horSpans = {-# SCC "horSpans" #-} traverse 0 (height-1) 3 (width-1) (0,-1,0,-2,0,-3)
        verSpans = {-# SCC "verSpans" #-} traverse 3 (height-1) 0 (width-1) (-1,0,-2,0,-3,0)
        incDiags = {-# SCC "incDiags" #-} traverse 0 (height-4) 0 (width-4) (1,1,2,2,3,3)
        decDiags = {-# SCC "decDiags" #-} traverse 3 (height-1) 0 (width-4) (-1,1,-2,2,-3,3)
        inc counts idx = {-# SCC "inc" #-} insertWith' (+) idx 1 counts
        traverse y0 yn x0 xn (dy1,dx1,dy2,dx2,dy3,dx3) counts = {-# SCC "traverse" #-}
              foldl'
                (\counts y ->
                  foldl'
                    (\counts x -> let score = scores!(y,x) + scores!(y+dy1,x+dx1) + scores!(y+dy2,x+dx2) + scores!(y+dy3,x+dx3)
                                  in inc counts (score+4))
                    counts [x0..xn])
                counts [y0..yn]
        coeff i = {-# SCC "coeff" #-} case i of
          0 -> -10; 1 -> -5; 2 -> -2; 3 -> -1
          5 -> 1; 6 -> 2; 7 -> 5; 8 -> 10
          _ -> 0

abMinimax :: Cell -> Int -> Board -> (Maybe Int, Score)
abMinimax color depth board
  | depth <= 0 = (Nothing, scoreBoard board)
  | otherwise =
    let scoresMoves = [(score, move)
                       | move <- [0..width-1], not ((0,move) `member` board),
                         let board' = dropDisk board move color,
                         let score = snd . abMinimax (otherColor color) (depth-1) $ board']
        (targetScore,pick) = case color of
              Orange -> (orangeWins, maximum)
              Yellow -> (yellowWins, minimum)
    in case find ((== targetScore) . fst) scoresMoves of
          Just (killerScore,killerMove) -> (Just killerMove, killerScore)
          _ -> let (score,move) = pick scoresMoves
               in (Just move, score)

loadBoard :: [String] -> Board
loadBoard = foldl' addCells empty
  where addCells board arg = case arg of
          'o':y:x:_ -> insert (digitToInt y, digitToInt x) Orange board
          'y':y:x:_ -> insert (digitToInt y, digitToInt x) Yellow board
          _         -> board

main = do
  board <- loadBoard `liftM` getArgs
  case scoreBoard board of
      score | score == orangeWins -> putStrLn "I win"
            | score == yellowWins -> putStrLn "You win"
            | otherwise -> let (mv, score) = abMinimax Orange maxDepth board
                           in case mv of
                              Just col -> print col
                              _        -> putStrLn "No move possible"
