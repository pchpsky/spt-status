module PlayerTime where

import DBus
import Data.Text (justifyRight)

data PlayerTime = MkPlayerTime Integer Integer deriving (Show)

instance IsVariant PlayerTime where
  toVariant pt = toVariant (toMicros pt :: Word64)

  fromVariant variant =
    ((fromVariant variant :: Maybe Word64) <&> fromMicros)
      <|> ((fromVariant variant :: Maybe Int64) <&> fromMicros)

fromMicros :: Integral a => a -> PlayerTime
fromMicros micros = MkPlayerTime (seconds `div` 60) (seconds `mod` 60)
  where
    seconds = (`div` 1000000) . toInteger $ micros

toMicros :: Integral a => PlayerTime -> a
toMicros (MkPlayerTime m s) = fromInteger $ (m * 60 + s) * 1000000

format :: PlayerTime -> Text
format (MkPlayerTime mins seconds) =
  justifyRight 2 '0' (show mins)
    <> ":"
    <> justifyRight 2 '0' (show seconds)
