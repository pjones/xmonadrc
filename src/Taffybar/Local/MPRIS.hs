{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Display the currently playing song.
module Taffybar.Local.MPRIS
       ( mprisConfig
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.List
import Data.Maybe
import System.Taffybar.MPRIS

--------------------------------------------------------------------------------
mprisConfig :: MPRISConfig
mprisConfig = MPRISConfig {trackLabel = display}

--------------------------------------------------------------------------------
display :: TrackInfo -> String
display track = "<span fgcolor='#b58900'>▶</span> " ++ info
  where
    info :: String
    info = case radio track of
      Just s  -> s
      Nothing -> intercalate " | " $ catMaybes [ trackArtist track
                                               , trackAlbum  track
                                               , trackTitle  track
                                               ]

--------------------------------------------------------------------------------
radio :: TrackInfo -> Maybe String
radio track = if isInfixOf "SomaFM" name
                then Just (clean name) else Nothing
  where
    name :: String
    name = intercalate " " $ catMaybes [ trackTitle  track
                                       , trackArtist track
                                       , trackAlbum  track
                                       ]

    clean :: String -> String
    clean = id
