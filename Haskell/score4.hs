module Main where

import Data.Map as M (Map, (!), findWithDefault, insert, insertWith', keys, empty, map, member, toList, foldrWithKey)
import Data.List (sort, foldl',find)
import Data.Char (digitToInt)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Prelude as P
--import Debug.Trace (trace)

(width,height) = (7,6)
maxDepth = 7
orangeWins = 1000000
yellowWins = -orangeWins

data Cell = Orange | Yellow deriving Eq
type Board = Map (Int,Int) Cell
type Score = Int

otherColor Orange = Yellow
otherColor Yellow = Orange

dropDisk :: Board -> Int -> Cell -> Board
dropDisk board column color | 0<row = insert (row-1, column) color board
                            | otherwise = error "full board"
  where row = fst . minimum . ((height,column) :) . filter ((== column) . snd) . keys $ board

scoreBoard :: Board -> Int
scoreBoard board | 0 `member` counts = yellowWins
                 | 8 `member` counts = orangeWins
                 | otherwise         = M.foldrWithKey (\i c s -> s + (coeff i) * c) 0 counts
  where counts = horSpans . verSpans . incDiags . decDiags $ empty
        scores = M.map rateCell board
        rateCell cell = case cell of
            Orange -> 1
            Yellow -> -1
        horSpans = traverse [0..height-1] [3..width-1] (\(y,x) -> [(y,x),(y,x-1),(y,x-2),(y,x-3)])
        verSpans = traverse [0..width-1] [3..height-1] (\(x,y) -> [(y,x),(y-1,x),(y-2,x),(y-3,x)])
        incDiags = traverse [0..height-4] [0..width-1] (\(y,x) -> [(y,x),(y-1,x-1),(y-2,x-2),(y-3,x-3)])
        decDiags = traverse [3..height-1] [0..width-4] (\(y,x) -> [(y,x),(y-1,x+1),(y-2,x+2),(y-3,x+3)])
        inc counts idx = insertWith' (+) idx 1 counts
        getScore k = findWithDefault 0 k -- k here to get around monomorphism restriction
        traverse as bs f counts = foldl'
                                    (\counts a ->
                                      foldl'
                                        (\counts b -> let score = sum [getScore pos scores | pos <- f (a,b)]
                                                      in inc counts (score+4))
                                        counts bs)
                                    counts as
        coeff i = case i of
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
    in case find (\(killerScore,_) -> killerScore == targetScore) scoresMoves of
          Just (killerScore,killerMove) -> (Just killerMove, killerScore)
          _ -> let (score,move) = pick scoresMoves
               in (Just move, score)

loadBoard :: [String] -> Board
loadBoard = foldl' addCells empty
  where addCells board arg = case arg of
          'o':cy:cx:_ -> insert (digitToInt cy, digitToInt cx) Orange board
          'y':cy:cx:_ -> insert (digitToInt cy, digitToInt cx) Yellow board
          _           -> board

main = do
  board <- loadBoard `liftM` getArgs
  case scoreBoard board of
      score | score == orangeWins -> putStrLn "I win"
            | score == yellowWins -> putStrLn "You win"
            | otherwise -> let (mv, score) = abMinimax Orange maxDepth board
                           in case mv of
                              Just col -> print col
                              _        -> putStrLn "No move possible"
