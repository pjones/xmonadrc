{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Utilities and configuration for manipulating music from within XMonad.
module XMonad.Local.Music (radioPrompt) where

--------------------------------------------------------------------------------
import Control.Exception
import Control.Monad (when, void)
import Control.Monad.Except (MonadError(..), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random (evalRandIO, uniform)
import qualified Data.ByteString as ByteString
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import qualified Network.MPD as MPD
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Playlist.HTTP.Full
import XMonad.Core
import XMonad.Local.Prompt (listCompFunc)
import XMonad.Prompt

--------------------------------------------------------------------------------
-- | A data type for the @XPrompt@ class.
data RadioStream = RadioStream

instance XPrompt RadioStream where
  showXPrompt RadioStream = "Stream: "

--------------------------------------------------------------------------------
-- | Full @FilePath@ to where I keep my radio station list.
radioStationFile :: IO FilePath
radioStationFile = (</> "documents/playlists/streams.m3u") <$> getHomeDirectory

--------------------------------------------------------------------------------
-- | Parse the radio station file and return the resulting playlist.
radioStationPlaylist :: IO (Either String Playlist)
radioStationPlaylist = do
  filepath <- radioStationFile

  case fileNameToFormat filepath of
    Nothing  -> return (Left "Invalid radio file.")
    Just fmt -> parsePlaylist fmt <$> ByteString.readFile filepath

--------------------------------------------------------------------------------
radioPrompt :: XPConfig -> X ()
radioPrompt c = do
  playlist' <- io $ radioStationPlaylist `catch` econst (Left "fail")

  case playlist' of
    Left _         -> return ()
    Right playlist -> go playlist


  where
    go :: Playlist -> X ()
    go playlist = mkXPrompt RadioStream c (comp playlist) (playStream playlist)

    comp :: Playlist -> ComplFunction
    comp = listCompFunc c . titles

    titles :: Playlist -> [String]
    titles = mapMaybe (fmap Text.unpack . trackTitle)

--------------------------------------------------------------------------------
playStream :: Playlist -> String -> X ()
playStream playlist title = do
    track <- runExceptT $ do
      url <- findTrack (Text.pack title)
      manager <- liftIO (newManager defaultManagerSettings)
      streams <- liftEither =<< download (env manager) url
      pickTrack streams
    case track of
      Left _  -> return ()
      Right t -> playURL (trackURL t)
  where
    findTrack :: (MonadError Error m) => Text -> m Text
    findTrack name =
      case find (\t -> trackTitle t == Just name) playlist of
        Nothing    -> throwError (InvalidURL name)
        Just track -> return (trackURL track)

    pickTrack :: (MonadIO m) => Playlist -> m Track
    pickTrack = liftIO . evalRandIO . uniform

    env :: Manager -> Environment
    env m = Environment m under5MB

    under5MB :: Int -> ByteStatus
    under5MB n = if n < 5242880 then Continue else LimitReached

--------------------------------------------------------------------------------
playURL :: Text -> X ()
playURL url = io . void . MPD.withMPD $ do
  current <- MPD.currentSong
  when (isStream current) $ deleteSong current
  MPD.addId (fromString $ Text.unpack url) Nothing >>= MPD.playId

--------------------------------------------------------------------------------
isStream :: Maybe MPD.Song -> Bool
isStream Nothing = False
isStream (Just song) = http || https where
  url   = MPD.toUtf8 (MPD.sgFilePath song)
  http  = "http://"  `ByteString.isPrefixOf` url
  https = "https://" `ByteString.isPrefixOf` url

--------------------------------------------------------------------------------
deleteSong :: Maybe MPD.Song -> MPD.MPD ()
deleteSong Nothing     = return ()
deleteSong (Just song) = do
  sid <- maybe (fail "no song ID!") return $ MPD.sgId song
  MPD.stop
  MPD.deleteId sid

--------------------------------------------------------------------------------
-- | Shamelessly stolen from:
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Prompt-Shell.html
econst :: Monad m => a -> IOException -> m a
econst = const . return
