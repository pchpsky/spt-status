module PlayerMetadata where

import DBus
import qualified Data.Map as M
import PlayerTime

data PlayerMetadata = MkPlayerMetadata
  { trackLength :: PlayerTime,
    trackId :: Text,
    album :: Text,
    artists :: [Text],
    title :: Text,
    url :: Text
  }
  deriving (Show)

instance IsVariant PlayerMetadata where
  toVariant MkPlayerMetadata {..} =
    toVariant $
      emptyMap
        & "mpris:length" `to` trackLength
        & "mpris:trackid" `to` trackId
        & "xesam:album" `to` album
        & "xesam:artist" `to` artists
        & "xesam:title" `to` title
        & "xesam:url" `to` url
    where
      to k v = M.insert k (toVariant v)
      emptyMap = M.empty :: M.Map String Variant

  fromVariant mapVariant =
    MkPlayerMetadata
      <$> lookup "mpris:length"
      <*> lookup "mpris:trackid"
      <*> lookup "xesam:album"
      <*> lookup "xesam:artist"
      <*> lookup "xesam:title"
      <*> lookup "xesam:url"
    where
      meta = fromVariant mapVariant :: Maybe (M.Map String Variant)
      lookup k = meta >>= M.lookup k >>= fromVariant
