{-|
Module      : Config
Description : The configuration for the program.
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3

The configuration for the game
-}

module Config where

import AI (AI, ais)
import Data.Maybe (fromMaybe)

-- | The different types of games (and help, which isn't really a game,
-- but is still a mode of operation for our program)
data GameType = Console | GUI | Tournament | NetHost | NetClient | Help String
  deriving (Eq, Show)

data PlayerType = Human
                | AI String -- Name of AI
  deriving (Eq, Show)

-- | The game configuration 
data Config 
  = HelpConfig       -- ^ Not really a game type, but a mode of operation
    String           -- The help message to print
  | ConsoleConfig    -- ^ A console game
    (Maybe AI)       -- Player 1 (Nothing if Human)
    (Maybe AI)       -- Player 2 (Nothing if Human)
    Double           -- Timeout in seconds
  | GUIConfig        -- ^ A game with graphics
    (Maybe AI)       -- Player 1 (Nothing if Human)
    (Maybe AI)       -- Player 2 (Nothing if Human)
    Double           -- Timeout in seconds
  | TournamentConfig -- ^ A game with a tournament server
    AI               -- Player to compete
    String           -- Location of tournament server
  | NetHostConfig    -- ^ Host a network game
    AI               -- Player
    Double           -- Timeout in seconds
    Integer          -- Port Number
  | NetClientConfig  -- ^ Join a network game
    AI               -- Player
    String           -- Host to connect to
    Integer          -- Port to connect on

-- | The different command line arguments that can be passed to the game
data Arg
  = GameType GameType
  | Timeout Double
  | Player1 PlayerType
  | Player2 PlayerType
  | Hostname String
  | PortNum Integer
  deriving (Eq, Show)

-- | Extract an element from a 0 or 1 element list, returning a default
-- value on the empty list, and throwing an error on any longer lists
extractOption
  :: Show a
  => [a]    -- ^ The list of values
  -> a      -- ^ The default value
  -> String -- ^ The name of the value (plural, for error messages)
  -> a
extractOption [] def _ = def
extractOption [x] _ _ = x
extractOption options _ name =
  error ("extractOption: Expected 0 or 1 "
          ++ name ++ ", but found " ++ show options)

-- | Make a game configuration from the arguments given.
makeConfig :: [Arg] -> Config 
makeConfig args = case getHelpMessage of
  Just message -> HelpConfig message -- We need to check for this first
  Nothing -> case gameType of
      Console -> ConsoleConfig (toPlayer player1) (toPlayer player2) timeout
      GUI -> GUIConfig (toPlayer player1) (toPlayer player2) timeout
      NetHost -> NetHostConfig (toAI player1) timeout portNum
      NetClient -> NetClientConfig (toAI player1) hostname portNum
      Tournament -> TournamentConfig (toAI player1) hostname
      -- Shouldn't come up, but otherwise we have a non-exhaustive patterns
      -- warning.
      Help s -> HelpConfig s 
                             
  where
    -- Because of lazy evaluation, not all of these will be evaluated.
    -- We use a rather cute trick with list comprehensions to filter based
    -- on patterns. 
    gameType = extractOption [g | GameType g <- args] GUI "game types"
    -- We check if there was a request for help anywhere inside the message
    -- since the other arguments don't matter if this was the case
    getHelpMessage = case [s | GameType (Help s) <- args] of
      [] -> Nothing
      x:_ -> Just x
    timeout = extractOption [t | Timeout t <- args] 4 "timeouts"
    player1 = extractOption [p | Player1 p <- args] Human "player 1s"
    player2 = extractOption [p | Player2 p <- args] (AI "helloWorld") "player 2s"
    hostname = extractOption [s | Hostname s <- args]
                 (error "makeConfig: No hostname given") "hostnames"
    portNum = extractOption [n | PortNum n <- args] 9001 "port numbers"
    findAI name = fromMaybe (error ("makeConfig: Couldn't find AI " ++ name))
                    (lookup name ais)
    toPlayer Human = Nothing
    toPlayer (AI name) = Just (findAI name)
    toAI Human = error "toAI: Can't use a Human here"
    toAI (AI name) = findAI name
    
