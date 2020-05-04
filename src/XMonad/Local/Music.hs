{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.
-}

-- | Utilities and configuration for manipulating music from within XMonad.
module XMonad.Local.Music (radioPrompt) where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO, uniform)
import Control.Monad.Reader
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (find)
import Data.List (isPrefixOf)
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Libnotify.C.Notify as Notify
import qualified Libnotify.C.NotifyNotification as Notify
import qualified Network.HTTP.Client as HTTP
import qualified Network.MPD as MPD
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Playlist.HTTP.Full
import XMonad.Core
import XMonad.Local.Prompt (listCompFunc)
import XMonad.Prompt

-- | A data type for the @XPrompt@ class.
data RadioStream = RadioStream
  { radioHttpManager :: HTTP.Manager,
    radioPromptCfg :: XPConfig
  }

instance XPrompt RadioStream where
  showXPrompt RadioStream {} = "Stream: "

type Radio = ExceptT String (ReaderT RadioStream X)

liftX :: X a -> Radio a
liftX = lift . lift

-- | Prompt the user to choose a track from the given playlist.
radioPromptForTrack :: Playlist -> Radio Track
radioPromptForTrack list = do
  r@RadioStream {radioPromptCfg} <- ask
  result <- liftX $ mkXPromptWithReturn r radioPromptCfg (comp radioPromptCfg) findTrack
  case join result of
    Nothing -> throwError "nothing selected"
    Just track -> pure track
  where
    comp :: XPConfig -> ComplFunction
    comp cfg = listCompFunc cfg (titles list)
    titles :: Playlist -> [String]
    titles = mapMaybe (fmap Text.unpack . trackTitle)
    findTrack :: String -> X (Maybe Track)
    findTrack stitle =
      let title = Text.pack stitle
       in pure $ find (\t -> trackTitle t == Just title) list

-- | If the given 'FilePath' is a streaming URL, send that to MPD.  If
-- it's a playlist, load the playlist.
radioPlayOrPrompt :: FilePath -> Radio ()
radioPlayOrPrompt path =
  case fileNameToFormat path of
    Nothing -> liftX (playUrl path)
    Just fmt -> do
      list <- loadPlaylist fmt path
      if any isPlaylist list
        then radioPromptForTrack list >>= radioPlayOrPrompt . Text.unpack . trackURL
        else pickTrack list >>= liftX . playUrl . Text.unpack . trackURL
  where
    isPlaylist :: Track -> Bool
    isPlaylist = isJust . fileNameToFormat . Text.unpack . trackURL
    pickTrack :: Playlist -> Radio Track
    pickTrack = liftIO . evalRandIO . uniform
    loadPlaylist :: Format -> FilePath -> Radio Playlist
    loadPlaylist fmt pathOrUrl
      | "http://" `isPrefixOf` pathOrUrl || "https://" `isPrefixOf` pathOrUrl =
        downloadPlaylist pathOrUrl >>= parse fmt
      | otherwise = liftIO (ByteString.readFile pathOrUrl) >>= parse fmt
    downloadPlaylist :: FilePath -> Radio ByteString
    downloadPlaylist url = do
      RadioStream {radioHttpManager} <- ask
      req <- maybe (throwError "bad url") pure $ HTTP.parseUrlThrow url
      liftIO $ (`catch` \(_ :: HTTP.HttpException) -> pure ByteString.empty)
        $ HTTP.withResponse req radioHttpManager
        $ \res -> do
          bs <- HTTP.brReadSome (HTTP.responseBody res) (2 * 1024 * 1024) -- 2MB
          pure (LByteString.toStrict bs)
    parse :: Format -> ByteString -> Radio Playlist
    parse fmt = either throwError pure . parsePlaylist fmt

-- | Full @FilePath@ to where I keep my radio station list.
radioStationFile :: IO FilePath
radioStationFile = (</> "documents/playlists/streams.m3u") <$> getHomeDirectory

radioPrompt :: XPConfig -> X ()
radioPrompt conf = do
  playlist <- liftIO radioStationFile
  manager <- liftIO (HTTP.newManager HTTP.defaultManagerSettings)
  let env = RadioStream manager conf'
  _ <- runReaderT (runExceptT (radioPlayOrPrompt playlist)) env
  pure ()
  where
    conf' :: XPConfig
    conf' = conf {alwaysHighlight = True}

-- | Play the given URL.
--
-- Overrides the MPD host because "localhost" turns into "::1" and
-- then a connection failure happens.
--
-- https://github.com/vimus/libmpd-haskell/issues/114
playUrl :: FilePath -> X ()
playUrl url = do
  result <- liftIO $ (`catch` onE) $ MPD.withMPD_ (Just "127.0.0.1") Nothing $ do
    current <- MPD.currentSong
    when (isStream current) $ deleteSong current
    MPD.addId (fromString url) Nothing >>= MPD.playId
  case result of
    Left e -> liftIO (notify "MPD Error" (show e))
    Right _ -> pure ()
  where
    onE :: IOException -> IO (Either MPD.MPDError a)
    onE = pure . Left . MPD.Unexpected . show

isStream :: Maybe MPD.Song -> Bool
isStream Nothing = False
isStream (Just song) = http || https
  where
    url = MPD.toUtf8 (MPD.sgFilePath song)
    http = "http://" `ByteString.isPrefixOf` url
    https = "https://" `ByteString.isPrefixOf` url

deleteSong :: Maybe MPD.Song -> MPD.MPD ()
deleteSong Nothing = return ()
deleteSong (Just song) = do
  sid <- maybe (fail "no song ID!") return $ MPD.sgId song
  MPD.stop
  MPD.deleteId sid

-- | Send a notification.
notify :: String -> String -> IO ()
notify summary body =
  bracket (Notify.notify_init "XMonad") (const Notify.notify_uninit) $ \_ -> do
    notice <- Notify.notify_notification_new summary body "info"
    Notify.notify_notification_set_timeout notice Notify.Default
    () <$ Notify.notify_notification_show notice
