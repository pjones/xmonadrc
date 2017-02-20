--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import System.Taffybar
import Taffybar.Local.Host

--------------------------------------------------------------------------------
main :: IO ()
main = do
  host <- getHostConfig

  taffybarMain defaultTaffybarConfig
    { startWidgets  = leftWidgets  host
    , endWidgets    = rightWidgets host
    , monitorNumber = primaryMonitorNum host
    , barHeight     = 20
    , barPosition   = Bottom
    }
