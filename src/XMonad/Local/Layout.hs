{-# OPTIONS -fno-warn-missing-signatures #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Layout configuration and hook.
module XMonad.Local.Layout (layoutHook) where

--------------------------------------------------------------------------------
import XMonad hiding ((|||), layoutHook, float)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.TwoPane (TwoPane(..))

--------------------------------------------------------------------------------
import XMonad.Local.Theme (decoTheme)

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook = avoidStruts $ boringWindows layouts

--------------------------------------------------------------------------------
-- | All of the layouts and layout modifiers that I use.  See the
-- documentation for @layoutHook@ above for information about the type
-- signature.
layouts =  floatF12 maxToggle where
  tall      = renamed [Replace "Tall"]  $ ResizableTall 1 (1.5/100) (3/5) []
  rtall     = renamed [Replace "RTall"] $ reflectHoriz tall
  two       = renamed [Replace "2Col"]  $ TwoPane (3/100) (3/5)
  three     = renamed [Replace "3Col"]  $ ThreeColMid 1 (3/100) (1/2)
  full      = renamed [Replace "Full"]  $ noBorders Full
  float     = renamed [Replace "Float"] simplestFloat
  floatF12  = onWorkspace "F12" float
  maxToggle = renamed [CutWordsLeft 1] $ maximize toggle
  toggle    = deco tall ||| deco rtall ||| deco three ||| deco two ||| full

--------------------------------------------------------------------------------
-- | Add simple decorations to windows.
deco = renamed [CutWordsLeft 1] . noFrillsDeco shrinkText decoTheme
