module Output where

import PlayerMetadata
import PlayerState

class OutputAlg m where
  applyFont :: Int -> Text -> m Text
  clickableGoPrev :: Text -> m Text
  clickableGoNext :: Text -> m Text
  clickablePlayPause :: Text -> m Text

formatArtist :: PlayerState -> Text
formatArtist ps =
  ps
    & metadata
    & chainedTo (viaNonEmpty head . artists)
    & fromMaybe ""

formatTitle :: PlayerState -> Text
formatTitle = maybe "" title . metadata

formatSong :: PlayerState -> Text
formatSong ps = formatTitle ps <> " - " <> formatArtist ps

sptIcon :: OutputAlg m => m Text
sptIcon = applyFont 3 "\61884"

openSpotify :: (OutputAlg m, Applicative m) => m Text
openSpotify = fmap (<>) sptIcon <*> applyFont 2 " Open Spotify"

formatControls :: (OutputAlg m, Monad m) => PlayerState -> m Text
formatControls MkPlayerState {..} = do
  goPrev <- clickableGoPrev $ if canGoPrevious then "\61512" else ""
  goNext <- clickableGoNext $ if canGoNext then "\61521" else ""
  playPause <-
    clickablePlayPause
      ( case playbackStatus of
          Playing -> "\61516"
          Paused -> "\61515"
          Stopped -> ""
      )
  applyFont 4 $ goPrev <> " " <> playPause <> " " <> goNext
