{-|
Module      : GameState
Description : The module for handling the game state, and operations over it
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3
-}
module GameState where

-- | There are two players, `Dark` and `Light`
data Player = Dark | Light
  deriving (Eq, Ord, Show)

-- | Return the opponent of a given player
opponent :: Player -> Player
opponent Dark = Light
opponent Light = Dark

-- | The Othello board is represented as a list of list of `Maybe Player`
-- where `Nothing` is an empty square. It should contain 8 lists, each
-- containing 8 elements.
type Board = [[Maybe Player]]

-- | Print the board in a human-readable format. It won't work as desired
-- directly inside of GHCi. To use it, try
-- > putStrLn (ppBoard yourBoard)
ppBoard :: Board -> String
ppBoard board = unlines ([ unwords (show n : map pieceToString row)
                         | (n,row) <- zip [1 :: Int ..] board]
                         ++ [unwords (map return (' ':['a'..'h']))])
  where
    pieceToString Nothing = "."
    pieceToString (Just Dark) = "x"
    pieceToString (Just Light) = "o"

-- | Are the coords given out of bounds?
outOfBounds :: Int -> Int -> Bool
outOfBounds colNum rowNum = not (0 <= colNum && colNum <= 7
  && 0 <= rowNum && rowNum <= 7)

-- | Throw an error if the coords are out of bounds, with
-- the coordinated and strings given
boundsError :: Int -> Int -> String -> a
boundsError colNum rowNum callee =
  error (concat [ callee, ": Invalid bounds (col=", show colNum
                , ") (row=", show rowNum, ")"
                ])

-- | Find the piece at the give coords
pieceAt :: Int -> Int -> Board -> Maybe Player
pieceAt colNum rowNum board
  | outOfBounds colNum rowNum = boundsError colNum rowNum "pieceAt"
  | otherwise = case drop rowNum board of
      [] -> error "pieceAt: Malformed Board"
      row:_ -> case drop colNum row of
        [] -> error "pieceAt: Malformed Board"
        piece:_ -> piece

-- | Replace the piece at the given coords with the provied piece
update :: Int -> Int -> Maybe Player -> Board -> Board
update colNum rowNum piece board
  | outOfBounds colNum rowNum = boundsError colNum rowNum "update"
  | otherwise = updateRow rowNum board
  where
    updateRow _ [] = error "update:updateRow: Index error"
    updateRow 0 (x:xs) = updateCol colNum x:xs
    updateRow n (x:xs) = x : updateRow (n-1) xs
    updateCol _ [] = error "update:updateCol: Index error"
    updateCol 0 (_:xs) = piece:xs
    updateCol n (x:xs) = x : updateCol (n-1) xs

-- | Change the coords given on the board to being owned by the player
changeTo :: Player -> [(Int,Int)] -> Board -> Board
changeTo _ [] board = board
changeTo p ((col,row):xs) board = changeTo p xs (update col row (Just p) board)

-- | Take several coords from the board, starting at the position given
-- a function which says whether to include this piece
-- a function which says whether we should stop
-- a function to give the next coords
-- and a function saying what to do to the list of coords if
-- we walk off the edge of the board.
collectPieces :: Int -> Int -> Board -> (Maybe Player -> Bool) ->
                 (Maybe Player -> Bool) ->
                 (Int -> Int -> (Int,Int)) ->
                 ([(Int,Int)] -> [(Int,Int)]) ->
                 [(Int,Int)] 
collectPieces colNum rowNum board keepTaking stopTaking nextCoord edgeFunction =
  go colNum rowNum []
  where
    go c r pieces
      | outOfBounds c r = edgeFunction pieces
      | keepTaking currentPiece = go c' r' ((c,r):pieces)
      | stopTaking currentPiece = pieces
      | otherwise = []
      where
        currentPiece = board !! r !! c
        (c', r') = nextCoord c r
