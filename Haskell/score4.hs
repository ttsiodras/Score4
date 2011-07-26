module Main where

import Data.Map as M (Map, insert, keys, empty, member, toList)
import Data.List (foldl',find)
import Data.Char (digitToInt)
import System.Environment (getArgs)
import Data.Array.IArray (accumArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.MArray (MArray, newArray, readArray, writeArray)
import Control.Monad.ST (ST)
import Debug.Trace (trace)

(width,height) = (7,6)
maxDepth = 7
(orangeWins,yellowWins) = (1000000,-orangeWins)
debug = True

data Cell = Orange | Yellow
type Board = Map (Int,Int) Cell
type Score = Int

otherColor Orange = Yellow
otherColor Yellow = Orange

dropDisk :: Board -> Int -> Cell -> Board
dropDisk board column color | 0 < oldRow = insert (oldRow-1, column) color board
                            | otherwise  = error "full board"
  where oldRow = fst . minimum . ((height,column) :) . filter ((== column) . snd) . keys $ board

scoreBoard :: Board -> Int
scoreBoard board | counts ! 0 /= 0 = yellowWins
                 | counts ! 8 /= 0 = orangeWins
                 | otherwise = counts!5 + 2*counts!6 + 5*counts!7 - counts!3 - 2*counts!2 - 5*counts!1

  where counts = runSTUArray $ do a <- newArray (0,8) 0
                                  traverse 0 (height-1) 3 (width-1)   0 (-1) a
                                  traverse 3 (height-1) 0 (width-1) (-1)  0  a
                                  traverse 0 (height-4) 0 (width-4)   1   1  a
                                  traverse 3 (height-1) 0 (width-4) (-1)  1  a
                                  return a

        scores :: UArray (Int,Int) Int
        scores = accumArray (\_ y -> rateCell y) 0 ((0,0),(height-1,width-1)) (toList board)

        rateCell cell = case cell of
            Orange -> 1
            Yellow -> -1

        traverse :: Int -> Int -> Int -> Int -> Int -> Int -> STUArray s Int Int -> ST s ()
        traverse y0 yn x0 xn dy dx counts = {-# SCC "traverse" #-}
            for y0 yn $ \y -> do
                for x0 xn $ \x -> let score = scores!(y,x) + scores!(y+dy,x+dx)
                                            + scores!(y+dy*2,x+dx*2) + scores!(y+dy*3,x+dx*3)
                                      idx = score + 4
                                  in readArray counts idx >>= writeArray counts idx . (+ 1)

        -- it's embarassing to have to write a custom version of forM_
        for lo hi proc | lo > hi = return ()
                       | otherwise = proc lo >> for (lo+1) hi proc

abMinimax :: Cell -> Int -> Board -> (Maybe Int, Score)
abMinimax color depth board
  | depth <= 0 = (Nothing, scoreBoard board)
  | otherwise =
    let scoresMoves = [(score, move)
                       | move <- [0..width-1], not ((0,move) `member` board),
                         let board' = dropDisk board move color,
                         let score = snd . abMinimax (otherColor color) (depth-1) $ board']
    in case find ((== targetScore) . fst) scoresMoves of
          Just (killerScore,killerMove) -> (Just killerMove, killerScore)
          _ -> let (score,move) = optimum scoresMoves
               in (if debug && depth==maxDepth then trace' scoresMoves else ()) -- just for debugging
                  `seq` (Just move, score)
  where trace' [] = ()
        trace' ((score,move):rest) = trace ("Depth "++(show depth)++", placing on "++(show move)++", score "++(show score))
                                           (trace' rest)
        (targetScore,optimum) = case color of
              Orange -> (orangeWins, maximum)
              Yellow -> (yellowWins, minimum)

loadBoard :: [String] -> Board
loadBoard = foldl' addCells empty
  where addCells board arg = case arg of
          'o':y:x:_ -> insert (digitToInt y, digitToInt x) Orange board
          'y':y:x:_ -> insert (digitToInt y, digitToInt x) Yellow board
          _         -> board

main = do
  board <- loadBoard `fmap` getArgs
  case scoreBoard board of
      score | score == orangeWins -> putStrLn "I win"
            | score == yellowWins -> putStrLn "You win"
            | otherwise -> let (mv, _) = abMinimax Orange maxDepth board
                           in case mv of
                              Just col -> print col
                              _        -> putStrLn "No move possible"
