module Scroll where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Default
import Prelude hiding (length)

data Scroll = MkScroll
  { length :: Int,
    position :: Int,
    separator :: Text,
    content :: Text
  }
  deriving (Eq, Show)

instance Default Scroll where
  def = MkScroll {length = 0, position = -1, separator = " ", content = ""}

tick :: Scroll -> Scroll
tick scroll@MkScroll {..} = scroll {position = (position + 1) `mod` T.length content}

format :: Scroll -> Text
format MkScroll {..}
  | T.length content <= length =
    toStrict $ takeT length $ toLazy content <> LT.repeat ' '
  | otherwise =
    toStrict $ takeT length . dropT position . LT.cycle $ toLazy (content <> separator)
  where
    takeT = LT.take . fromIntegral
    dropT = LT.drop . fromIntegral

reset :: Scroll -> Scroll
reset scroll = scroll {position = -1}
