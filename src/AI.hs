{-|
Module      : AI
Description : The AI for Othello
Copyright   : (c) Robert 'Probie' Offner, <Ra>, 2018
License     : GPL-3
-}
module AI where

import Game
import GameState
import Data.List


-- |  An AI is a function from the time given and the current state of
-- the game to a (potentially infinite) list of increasingly better
-- moves.
type AI = Double -> Game -> [(Int, Int)]

-- | A list of known AIs and their names.
ais :: [(String, AI)]
ais = [("default",         makeBestMove),    -- as required by specs strongest put under "default" name
       ("helloWorld",      makeGoodMove)]    -- second strongest AI

-- | A list of possible moves for the players.
possibleMoves :: [(Int,Int)]
possibleMoves = [(row,col) | row <- [0..7], col <- [0..7]]

-- | A list of valid moves from all possible moves for a given game.
legalMoves :: Game -> [(Int,Int)]
legalMoves (Game Nothing _) = [] -- The game is over
legalMoves (Game (Just player) board) =
  filter legalForThisGame possibleMoves
  where
    legalForThisGame (row,col) = legalMove row col player board

-- | Pick the first legal move for the game.
makeFirstLegalMove :: AI
makeFirstLegalMove _timeout game = case legalMoves game of
  [] -> error "makeFirstLegalMove: No Legal moves"
  move':_ -> [move']


game_to_game1 :: Game -> Game1 -- transformation from default state of the world data type to operational type
game_to_game1 (Game player board) = Game1 player board (3,4)

data Game1 = Game1 {current_player:: Maybe Player, -- upgraded state of the world data type. record
                    current_board :: Board,
                    move_made :: (Int,Int)} -- the move which has been done to result in current board
                    deriving (Show, Eq, Ord)
data Tree a = Node a [Tree a] -- general multy-way tree type

type Side = Player -- new name for already used type to indicate the purpose in function type signature. beneficiary party

make_children :: Game1 -> [Game1] --calculates all possible one step future game state. Game tree creation method
make_children (Game1 maybe_player board _ )=
    case maybe_player of
    Nothing -> []
    Just player -> [(Game1 (Just (opponent player)) (playMove x y player board) (x,y))|(x,y)<-legalMoves (Game (Just player) board)]

gameTree :: (a -> [a]) -> a -> Tree a -- general way to create a tree with given seed and method called f.
gameTree f a = Node a (map (gameTree f) (f a))

makeGameTree :: Game1 -> Tree Game1 -- particular tree creation using the above general functions
makeGameTree game = gameTree make_children game

weights :: [[Int]] -- symmetric average of best evolved Neural Network machine score map, for reference see report.
weights = [[ 100,    -35,    53,   -1,  -1,   53,   -35,   100],
           [ -35,    -69,   -22,  -10, -10,  -22,   -69,   -35],
           [  53,    -22,     8,    2,   2,    8,   -22,    53],
           [  -1,    -10,     2,    1,   1,    2,   -10,    -1],
           [  -1,    -10,     2,    1,   1,    2,   -10,    -1],
           [  53,    -22,     8,    2,   2,    8,   -22,    53],
           [ -35,    -69,   -22,  -10, -10,  -22,   -69,   -35],
           [ 100,    -35,    53,   -1,  -1,   53,   -35,   100]]

--reference Learning Board Evaluation Function for Othello by Hybridizing Convolution with Temporal Difference Learning
--https://www.researchgate.net/publication/233530017_Learning_Board_Evaluation_Function_for_Othello_by_Hybridizing_Coevolution_with_Temporal_Difference_Learning

-- calculates the strategic value of the bord. multiplies beneficiary's player score by one and subtract opponent's score
weightedScore :: Side -> Board -> Int -- strategic biased weight board value complement evaluation
weightedScore side board = sum [(2*score*coef) | (score, coef)<- (concat (zipWith zip weights (board_transform side board)))]

-- combines mobility factor and strategic value of the board
board_value :: Side -> Board -> Int -- combines two complements into one value
board_value side board = (weightedScore side board) + (mobilityEvaluation side board) -- combined value of the board

-- transforms the board to  coefficient coefficient board (0,1,-1) to multiply on it in weightedScore
board_transform :: Side -> Board -> [[Int]]
board_transform  side board =
    case board of
        [] -> []
        line:ls -> (line_transform side line): (board_transform side ls)
-- helper to the above function, goes into lines
line_transform :: Side->[Maybe Player] -> [Int]
line_transform side line =
    case line of
        [] -> []
        p:ps
            |p == Just side -> 1:line_transform side ps
            |p == Just (opponent side) -> (-1):(line_transform side ps)
            | otherwise -> 0:line_transform side ps

mobilityEvaluation :: Side -> Board -> Int -- evaluates the mobility factor of future positions
mobilityEvaluation side board =
    case side of
        Dark  -> 15*(dark_mobility - light_mobility) -- coefficients are different because the initial positions are different
        Light -> 17*(light_mobility - dark_mobility) -- coef. regulates the amount of impact of this factor on decision
        where
            dark_mobility = length (legalMoves (Game (Just Dark) board))
            light_mobility = length (legalMoves (Game (Just Light) board))


--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--Alpha Beta Minimax without heuristic--

-- | The default AI. It just repeatedly applies `makeAMove` to
-- increasingly higher depths.

makeGoodMove :: AI
makeGoodMove _timeout game = map (alfa game) [1..]

alfa :: Game -> Depth -> Move -- alpha beta AI play any colour, without heuristic it sees further ahead
alfa game@(Game side _ ) depth =
    case side of
        Just side'  ->   find_move_ab side' currentScore (game_to_game1 game) depth --
        _           ->  error "Game Over"



--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--Alpha Beta Minimax with heuristic--
type Alpha = Int
type Beta = Int
type Depth = Int
type Move = (Int,Int)
type Score =Int

-- | The default AI. It just repeatedly applies `makeAMove` to
-- increasingly higher depths.
makeBestMove :: AI
makeBestMove _timeout game = map (alfaPlus game) [1..]

alfaPlus :: Game -> Depth -> Move -- alpha beta bot mother function, play any colour with heuristic its smarter but sees less ahead
alfaPlus  game@(Game side _ ) depth =
    case side of
        Just side' -> find_move_ab side' board_value (game_to_game1 game) depth -- depth+1 is the amount of steps we look ahead
        _          -> error "Game Over"

find_move_ab :: Side -> (Side->Board->Int)-> Game1-> Depth-> Move -- another medium function extracts move from calculation
find_move_ab side eval_method game depth = best_move -- provision made to easily change the evaluation method
    where
        children = make_children game -- creates a list if possible outcomes
        score_list = map (alphabeta side eval_method depth (-10000) 10000) children -- depth starts from children so in human sense the total depth will be greater by one.
        --apllies function alphabeta on every option and receives a list of values from alphabeta
        --so the depth starts from here, the current state (root node in game tree) is not counted as a level in the depth
        move_list = map move_made children -- extracts moves from each child and puts them into a separate list
        m = maximum score_list -- finds the maximum score in the list
        best_index = elemIndices m score_list -- gts the list indexes of maximums of the list
        best_move = (!!) move_list (head best_index) -- calls the first maximum from the list by its number

alphabeta :: Side->(Side->Board->Int)->Depth->Alpha->Beta-> Game1-> Score -- core calculation of the alpha beta pruned tree
alphabeta  side eval_method depth  alpha beta game
    | (current_player game == Nothing) = who_won side game -- end of the game check heuristic (see report)
    | (depth == 0) = eval_method side (current_board game) -- reached the required depth evaluate
    | (current_player game == Just side) = maximise_ab side eval_method (make_children game) depth alpha beta (-10000)
    --decides depending on the player maximise or minimise, and builds the tree down more, sets score to virtual minus or plus infinity
    | (current_player game == Just (opponent side)) = minimise_ab side eval_method (make_children game) depth alpha beta 10000
    | otherwise = error "Game Over"

maximise_ab :: Side-> (Side->Board->Int)->[Game1] -> Depth-> Alpha-> Beta-> Score-> Score
maximise_ab side eval_method game depth alpha beta score = -- models the beneficiary party play
    case game of
        [] -> score
        g:gs
            | (new_alpha >= beta) -> new_score -- if alpha beta overlap then return value and dont look further down
            | otherwise -> maximise_ab side eval_method gs depth new_alpha beta new_score -- dig down updating alpha if one of the value of subtrees are greater than the current alpha
            where
                new_score = max (alphabeta side eval_method  (depth-1) alpha beta g) score -- mutual recursion to look down for the highest value
                new_alpha = max alpha new_score -- updates alpha if a greater value found

minimise_ab :: Side-> (Side->Board->Int)->[Game1] -> Depth-> Alpha-> Beta-> Score-> Score
minimise_ab side eval_method game depth alpha beta score = -- models the opponent play
    case game of
        [] -> score
        g:gs
            | (alpha >= new_beta) -> new_score -- again if alpha and beta overlap then stop looking
            | otherwise -> minimise_ab side eval_method gs depth alpha new_beta new_score -- or dig down further
            where
                new_score = min (alphabeta side eval_method  (depth-1) alpha beta g) score -- mutual recursion to look down for the smallest value
                new_beta = min beta new_score --updates beta if found smaller downstairs

who_won :: Side -> Game1 -> Score -- shifts priorities tiwards winning outcomes and away from loosing outcomes
who_won side game
    |dif>0 = 1000
    | dif <0 = (-1000)
    | otherwise = 0
        where
            oppScore = currentScore (opponent side) (current_board game)
            score = currentScore side (current_board game)
            dif = score - oppScore











