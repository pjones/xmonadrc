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
module XMonad.Local.Music (musicPrompt) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Exception
import Control.Monad (when, void)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.MPD as MPD
import Prelude hiding (catch)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.ParserCombinators.Parsec (parseFromFile)
import Text.Playlist
import XMonad.Core
import XMonad.Prompt

--------------------------------------------------------------------------------
-- | A data type for the @XPrompt@ class.
data RadioStream = RadioStream

instance XPrompt RadioStream where
  showXPrompt RadioStream = "Stream: "

--------------------------------------------------------------------------------
-- | Full @FilePath@ to where I keep my radio station list.
radioStationFile :: IO FilePath
radioStationFile = (</> "develop/pmade/rc/mpd/misc/streams") <$> getHomeDirectory

--------------------------------------------------------------------------------
-- | Parse the radio station file and return the resulting playlist.
radioStationPlaylist :: IO Playlist
radioStationPlaylist = do
  path   <- radioStationFile
  parsed <- parseFromFile simplePlaylist path
  either (fail . show) return parsed

--------------------------------------------------------------------------------
musicPrompt :: XPConfig -> X ()
musicPrompt c = do
  playlist <- io $ radioStationPlaylist `catch` econst []
  let titles = map T.unpack $ playlistTitles playlist
  mkXPrompt RadioStream c (mkComplFunFromList' titles) $ playStream playlist

--------------------------------------------------------------------------------
playStream :: Playlist -> String -> X ()
playStream pl title = maybe (return ()) playURL $ playlistLookup pl $ T.pack title

--------------------------------------------------------------------------------
playURL :: T.Text -> X ()
playURL url = io . void . MPD.withMPD $ do
  current <- MPD.currentSong
  when (isStream current) $ deleteSong current
  MPD.addId (MPD.Path $ T.encodeUtf8 url) Nothing >>= MPD.playId

--------------------------------------------------------------------------------
isStream :: Maybe MPD.Song -> Bool
isStream Nothing = False
isStream (Just song) = http || https where
  (MPD.Path path) = MPD.sgFilePath song
  http  = "http://"  `B.isPrefixOf` path
  https = "https://" `B.isPrefixOf` path

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
