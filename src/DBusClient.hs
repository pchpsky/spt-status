module DBusClient where

import DBus
import DBus.Client (Client, call)
import PlayerState

getPlayerState :: (MonadIO m) => Client -> Text -> m (Maybe PlayerState)
getPlayerState client playerName = do
  response <- liftIO $ call client (getPlayerPropsCall playerName)
  response
    & rightToMaybe
    & chainedTo (listToMaybe . methodReturnBody)
    & chainedTo fromVariant
    & return

getPlayerPropsCall :: Text -> MethodCall
getPlayerPropsCall player =
  (methodCall "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "GetAll")
    { methodCallDestination = Just mprisDest,
      methodCallBody = [toVariant ("org.mpris.MediaPlayer2.Player" :: Text)]
    }
  where
    mprisDest = busName_ . toString $ "org.mpris.MediaPlayer2." <> player
