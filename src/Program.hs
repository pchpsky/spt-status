module Program where

import PlayerState

type PlayerName = Text

class ProgramAlg m where
  getPlayerState :: PlayerName -> m (Maybe PlayerState)
  outputPlayerState :: PlayerState -> m ()
  outputNoPlayer :: m ()

runProgram :: (Monad m, ProgramAlg m) => PlayerName -> m ()
runProgram playerName = do
  playerState <- getPlayerState playerName
  maybe
    outputNoPlayer
    (\ps -> outputPlayerState ps *> runProgram playerName)
    playerState
