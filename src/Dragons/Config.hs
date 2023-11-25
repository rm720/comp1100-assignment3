{-# LANGUAGE LambdaCase #-}
{-|
Module      : Dragons.Config
Description : Read game configuration from args
Copyright   : (c) Robert 'Probie' Offner, 2018
License     : GPL-3

Parse the game configuration from command line arguments for Othello
-}

module Dragons.Config (parseGameConfig) where

import Config
import System.Console.GetOpt
import System.Environment

-- | Create a game config based on the command line arguments
-- to the program.
parseGameConfig :: IO Config
parseGameConfig = do
  programArgs <- getArgs
  case getOpt Permute options programArgs of
    (args, [], []) -> return (makeConfig args)
    (_,s@(_:_),_) -> error ("parseGameConfig: Didn't know what to do with: "
                            ++ show s ++ "\n" ++ usage)
    (_, _, errs) -> error ("parseGameConfig: Got errors:\n" ++ concat errs
                           ++ "\n" ++ usage)
  where
    options =
      [ Option ['T'] ["type"] (ReqArg (GameType . toGameType) "GAMETYPE")
          "The type of the game. One of console, gui, host,\
          \ join and tournament. Defaults to gui."
      , Option ['t'] ["timeout"] (ReqArg (Timeout . toTimeout) "TIMEOUT")
          "The timeout (in seconds) for the game."
      , Option ['p'] ["player1"] (ReqArg (Player1 . toPlayer) "PLAYER1")
          "Player 1 (or the only player for single player games).\
          \ HUMAN if a human player. Defaults to human."
      , Option ['P'] ["player2"] (ReqArg (Player2 . toPlayer) "PLAYER2")
          "Player 2 (HUMAN if a human player). Defaults to the AI helloWorld."
      , Option ['h'] ["hostname"] (ReqArg Hostname "HOSTNAME")
          "Hostname of the computer to connect to for a network game\
          \ or tournament."
      , Option ['n'] ["port"] (ReqArg (PortNum . toPortNum) "PORTNUMBER")
          "The port number to connect to or host on for a network game.\
          \ Defaults to 9001."
      , Option ['h'] ["help"] (NoArg (GameType (Help usage)))
          "Prints this help message and exits."
      ]
    usage = usageInfo "Usage: othello [OPTION...]" options
    toGameType = \case 
      "console" -> Console
      "gui" -> GUI
      "host" -> NetHost
      "join" -> NetClient
      "tournament" -> Tournament
      t -> error ("toGameType: Unknown game type " ++ t)
    toTimeout s = case reads s of
      [(t,"")] -> t
      _ -> error ("toTimeout: Can't read timeout " ++ s)
    toPlayer "HUMAN" = Human
    toPlayer ai = AI ai
    toPortNum s = case reads s of
      [(n,"")] -> n
      _ -> error ("toPortNum: Can't read port number" ++ s)
