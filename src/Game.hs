{-|
Module      : Game
Description : The module for implementing the game logic of Othello
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3
-}
module Game where

import Data.Maybe (isJust)
import GameState

-- | A `Game` contains the current `Board` and the player who's turn it
-- is (`Nothing` if the game is over)
data Game = Game (Maybe Player) Board
  deriving (Eq, Ord, Show)

-- | Who's turn is it? Return `Nothing` if the game is over
turn :: Game -> Maybe Player
turn (Game t _) = t

-- | Print the game in a human-readable format. It won't work as desired
-- directly inside of GHCi. To use it, try
-- > putStrLn (ppGame yourGame)
ppGame :: Game -> String
ppGame (Game currentTurn board) = unlines [ppBoard board, scoreLine]
  where
    scoreLine = unlines [ ppTurn currentTurn
                        , "Dark:\t" ++ show darkScore
                        , "Light:\t" ++ show lightScore
                        ]
    lightScore = currentScore Light board
    darkScore = currentScore Dark board
    ppTurn (Just Light) = "Turn:\tLight (o)"
    ppTurn (Just Dark) = "Turn:\tDark (x)"
    ppTurn Nothing = case compare lightScore darkScore of
      LT -> "Dark wins!"
      EQ -> "Draw"
      GT -> "Light wins!"

-- | The initial board for Othello
initialBoard :: Board
initialBoard = [[pieceOn colNum rowNum | colNum <- [0..7]]
               | rowNum <- [0..7]]
  where
    pieceOn :: Integer -> Integer -> Maybe Player
    pieceOn 3 3 = Just Light
    pieceOn 4 3 = Just Dark
    pieceOn 3 4 = Just Dark
    pieceOn 4 4 = Just Light
    pieceOn _ _ = Nothing

-- | The initial game state for Othello
initialGame :: Game
initialGame = Game (Just Dark) initialBoard

-- | Calculate the current score for a player
currentScore :: Player -> Board -> Int
currentScore piece board = length [p | Just p <- concat board, p == piece]

-- | Check if a move is legal
legalMove :: Int -> Int -> Player -> Board -> Bool
legalMove colNum rowNum player board
  | outOfBounds colNum rowNum = boundsError colNum rowNum "legalMove"
  | isJust (board !! rowNum !! colNum) = False
  | otherwise = currentScore player board + 1 <
      currentScore player (playMove colNum rowNum player board)

-- | Play a move as the player given. It does not check if the move is legal
playMove :: Int -> Int -> Player -> Board -> Board
playMove colNum rowNum player board
  | outOfBounds colNum rowNum = boundsError colNum rowNum "playMove"
  | otherwise = changeTo player ((colNum,rowNum):changedPieces) board
  where
    directions = [ (stepFun, stepFun colNum rowNum)
                 | stepX <- [pred, id, succ]
                 , stepY <- [pred, id, succ]
                 , let stepFun x y = (stepX x, stepY y)
                 , stepFun 0 0 /= (0,0) -- We need to move somewhere
                 ]
    isOpponent (Just p) = player == opponent p
    isOpponent _ = False
    changedPieces = [coord | (stepFun, (col, row)) <- directions
                           , coord <- collectPieces col row board isOpponent
                                        (== Just player) stepFun dropList]
    dropList _ = []  -- Drop the current list and return the empty list

-- | Make a move as the current player in the game. Returns `Nothing`
-- if the move was an illegal move.
play :: Int -> Int -> Game -> Maybe Game
play _ _ (Game Nothing _) = Nothing
play colNum rowNum (Game (Just p) b)
  | legalMove colNum rowNum p b = Just (Game p' b')
  | otherwise = Nothing
  where
    b' = playMove colNum rowNum p b
    -- If after this move, the opponent has at least one legal move
    -- then it's their turn. Otherwise, if the current player has a
    -- legal move, it stays their turn. If no-one has a legal move
    -- the game is over
    p' | or [ legalMove x y (opponent p) b'
            | x <- [0..7], y <- [0..7]] = Just (opponent p) 
       | or [ legalMove x y p b'
            | x <- [0..7], y <- [0..7]] = Just p
       | otherwise = Nothing
