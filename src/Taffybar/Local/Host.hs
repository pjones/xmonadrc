{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Per-host configuration for Taffybar.
module Taffybar.Local.Host
       ( Host (..)
       , getHostConfig
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Network.HostName

--------------------------------------------------------------------------------
-- | Host configuration.
data Host = Host
  { primaryMonitorNum :: Int
  , maxWindowTitle    :: Int
  } deriving Show

--------------------------------------------------------------------------------
-- | Get the proper configuration for the current host.
getHostConfig :: IO Host
getHostConfig = do
  name <- getHostName
  return $ case name of
    "holmwood" -> hostHolmwood
    _          -> hostDefault

--------------------------------------------------------------------------------
-- | Default configuration (for my desktop).
hostDefault :: Host
hostDefault =  Host
  { primaryMonitorNum = 1
  , maxWindowTitle    = 40
  }

--------------------------------------------------------------------------------
-- | My primary laptop.
hostHolmwood :: Host
hostHolmwood =  Host
  { primaryMonitorNum = 0
  , maxWindowTitle    = 30
  }
