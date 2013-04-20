--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | XMonad.Prompt configuration and utilities.
module XMonad.Local.Prompt (promptConfig) where

--------------------------------------------------------------------------------
-- XMonad contrib (Prompt)
import XMonad.Prompt

--------------------------------------------------------------------------------
promptConfig :: XPConfig
promptConfig = defaultXPConfig
  { position = Bottom
  , font     = "xft:dejavu sans mono:size=9"
  }
