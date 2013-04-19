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
