module PlayerState where

import Control.Monad.Except
import DBus
import qualified Data.Map as M
import Default
import PlayerMetadata
import PlayerTime

newtype PlayerError = PlayerError {getPlayerError :: String} deriving (Show)

type PlayerM = Except PlayerError

data PlaybackStatus = Playing | Paused | Stopped deriving (Show, Read, Eq)

instance IsVariant PlaybackStatus where
  toVariant s = toVariant (show s :: Text)
  fromVariant = fromVariant >=> readMaybe

data LoopStatus = None | Track | Playlist deriving (Show, Read)

data PlayerState = MkPlayerState
  { playbackStatus :: PlaybackStatus,
    shuffle :: Bool,
    position :: PlayerTime,
    canGoNext :: Bool,
    canGoPrevious :: Bool,
    canPlay :: Bool,
    canPause :: Bool,
    metadata :: Maybe PlayerMetadata
  }
  deriving (Show)

instance IsVariant PlayerState where
  toVariant MkPlayerState {..} =
    toVariant $
      emptyMap
        & "PlaybackStatus" `to` toVariant playbackStatus
        & "Shuffle" `to` toVariant shuffle
        & "Position" `to` toVariant position
        & "CanGoNext" `to` toVariant canGoNext
        & "CanGoPrevious" `to` toVariant canGoPrevious
        & "CanPlay" `to` toVariant canPlay
        & "CanPause" `to` toVariant canPause
        & "Metadata" `to` maybe (toVariant emptyMap) toVariant metadata
    where
      to = M.insert
      emptyMap = M.empty :: M.Map String Variant

  fromVariant mapVariant =
    MkPlayerState
      <$> lookup "PlaybackStatus"
      <*> lookup "Shuffle"
      <*> lookup "Position"
      <*> lookup "CanGoNext"
      <*> lookup "CanGoPrevious"
      <*> lookup "CanPlay"
      <*> lookup "CanPause"
      <*> pure metadata
    where
      player = fromVariant mapVariant :: Maybe (M.Map String Variant)
      lookup k = player >>= M.lookup k >>= fromVariant
      metadata =
        if lookup "PlaybackStatus" == pure Stopped
          then empty
          else lookup "Metadata"

instance Default PlayerState where
  def =
    MkPlayerState
      { playbackStatus = Stopped,
        shuffle = False,
        position = MkPlayerTime 0 0,
        canGoNext = False,
        canGoPrevious = False,
        canPlay = False,
        canPause = False,
        metadata = Nothing
      }

getTrackId :: PlayerState -> Maybe Text
getTrackId s = metadata s <&> trackId

formatTime :: PlayerState -> Text
formatTime MkPlayerState {..} =
  mconcat
    [ PlayerTime.format position,
      "/",
      PlayerTime.format (maybe (MkPlayerTime 59 59) trackLength metadata)
    ]
