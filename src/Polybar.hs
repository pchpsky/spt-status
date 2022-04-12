module Polybar where

wrap :: Text -> Text -> Text -> Text
wrap pre post = (pre <>) . (<> post)

font :: Int -> Text -> Text
font i = wrap (wrap "%{T" "}" (show i)) "%{T-}"

clickable :: Text -> Text -> Text
clickable command = wrap (wrap "%{A1:" ":}" command) "%{A}"
