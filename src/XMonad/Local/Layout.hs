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
import XMonad hiding (layoutHook, float)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.ToggleLayouts (toggleLayouts)

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook = avoidStruts $ boringWindows $ layouts

--------------------------------------------------------------------------------
-- | All of the layouts and layout modifiers that I use.  See the
-- documentation for @layoutHook@ above for information about the type
-- signature.
layouts = onWorkspace "F12" float $ toggleLayouts full (tall ||| three)
  where tall  = named "Tall"  $ ResizableTall 1 (1.5/100) (2/3) []
        full  = named "Full"  $ noBorders Full
        three = named "3Col"  $ ThreeColMid 1 (3/100) (1/2)
        float = named "Float" $ simplestFloat
