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
module XMonad.Local.Layout (layoutHook, selectLayoutByName) where

--------------------------------------------------------------------------------
import XMonad hiding ((|||), layoutHook, float)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Column
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Local.Theme
import XMonad.Prompt

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook = avoidStruts layouts

--------------------------------------------------------------------------------
-- | All of the layouts and layout modifiers that I use.  See the
-- documentation for @layoutHook@ above for information about the type
-- signature.
layouts = maxToggle . hiddenMod $ allLays  where
  tall       = renamed [Replace "Tall"]  (ResizableTall 1 (1.5/100) (3/5) [])
  two        = renamed [Replace "2Col"]  twoCols
  rtwo       = renamed [Replace "R2Col"] (reflectHoriz twoCols)
  accordion  = renamed [Replace "Acc"]   Accordion
  raccordion = renamed [Replace "RAcc"]  (Mirror Accordion)
  full       = renamed [Replace "Full"]  (noBorders Full)
  bspace     = renamed [Replace "BSP"]   emptyBSP
  maxToggle  = renamed [CutWordsLeft 1] . maximizeWithPadding 0
  hiddenMod  = renamed [CutWordsLeft 1] . hiddenWindows
  allLays    = bspace ||| tall      ||| two        ||| rtwo |||
               full   ||| accordion ||| raccordion

--------------------------------------------------------------------------------
-- | Only allow two visible windows.  The screen is split into two
-- layouts, the left side holds a single window, the right holds the
-- remaining windows in tabs.
twoCols = layoutLeft layoutRight where
  leftHalf    = relBox 0.0 0.0 0.5 1.0
  rightHalf   = relBox 0.5 0.0 1.0 1.0
  fullScreen  = relBox 0.0 0.0 1.0 1.0
  layoutLeft  = layoutN 1 leftHalf (Just fullScreen) (Column 1.6)
  layoutRight = layoutAll rightHalf (tabbedBottom shrinkText tabColors)

--------------------------------------------------------------------------------
-- | A data type for the @XPrompt@ class.
data LayoutByName = LayoutByName

instance XPrompt LayoutByName where
  showXPrompt LayoutByName = "Layout: "

--------------------------------------------------------------------------------
-- | Use @Prompt@ to choose a layout.
selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf =
  mkXPrompt LayoutByName conf (aListCompFunc conf layoutNames) go

  where
    go :: String -> X ()
    go selected = case lookup selected layoutNames of
                    Just name -> sendMessage (JumpToLayout name)
                    Nothing   -> return ()

    layoutNames :: [(String, String)]
    layoutNames = [ ("Accordion",              "Acc")
                  , ("Rotated Accordion",      "RAcc")
                  , ("Tall",                   "Tall")
                  , ("Two Column",             "2Col")
                  , ("Rotated Two Column",     "R2Col")
                  , ("Full Screen",            "Full")
                  , ("Binary Space Partition", "BSP")
                  ]
