module Main where

import Control.Concurrent
import qualified Control.Monad.State as S
import DBus.Client
import DBusClient
import Default
import Output
import PlayerState
import Polybar as P
import Program
import Scroll
import Shelly.Lifted

data AppError
  = DBusError String
  | PlayerStateError String
  deriving (Show)

data AppState = AppState {playerState :: PlayerState, scroll :: Scroll}

instance Default AppState where
  def = AppState def def

newtype AppEnv = AppEnv {dbusClient :: Client}

newtype AppM a = AppM {unAppM :: ReaderT AppEnv (S.StateT AppState Sh) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppEnv,
      MonadState AppState,
      MonadSh
    )

runAppM :: AppM a -> AppEnv -> AppState -> Sh a
runAppM app config = S.evalStateT (runReaderT (unAppM app) config)

instance OutputAlg AppM where
  applyFont i = pure . P.font i
  clickableGoPrev = pure . P.clickable "playerctl -p spotify previous"
  clickableGoNext = pure . P.clickable "playerctl -p spotify next"
  clickablePlayPause = pure . P.clickable "playerctl -p spotify play-pause"

instance ProgramAlg AppM where
  outputNoPlayer = openSpotify & chainedTo echo

  getPlayerState playerName = do
    client <- asks dbusClient
    DBusClient.getPlayerState client playerName

  outputPlayerState newState = do
    modify
      ( \ps ->
          if getTrackId (playerState ps) == getTrackId newState
            then AppState newState $ tick (scroll ps)
            else AppState newState $ makeTitleScroll newState
      )
    S.get >>= printOutput

makeTitleScroll :: PlayerState -> Scroll
makeTitleScroll playerState =
  def
    { Scroll.length = 30,
      content = formatSong playerState,
      separator = " - "
    }

printOutput :: AppState -> AppM ()
printOutput (AppState {..}) = do
  controls <- formatControls playerState
  formattedScroll <- scroll & format & applyFont 2
  _ <- shelly $ echo $ controls <> " " <> formattedScroll
  liftIO $ threadDelay 500000

main :: IO ()
main = do
  client <- connectSession
  shelly $ runAppM (runProgram "spotify") (AppEnv client) def
