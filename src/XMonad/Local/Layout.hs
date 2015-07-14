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
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Prompt

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook = avoidStruts layouts

--------------------------------------------------------------------------------
-- | All of the layouts and layout modifiers that I use.  See the
-- documentation for @layoutHook@ above for information about the type
-- signature.
layouts =  floatF12 . maxToggle . hiddenMod $ allLays  where
  tall      = renamed [Replace "Tall"]  $ ResizableTall 1 (1.5/100) (3/5) []
  two       = renamed [Replace "2Col"]  $ TwoPane (3/100) (3/5)
  accordion = renamed [Replace "Acc"]   $ Mirror Accordion
  full      = renamed [Replace "Full"]  $ noBorders Full
  float     = renamed [Replace "Float"] simplestFloat
  bspace    = renamed [Replace "BSP"] emptyBSP
  floatF12  = onWorkspace "F12" float
  maxToggle = renamed [CutWordsLeft 1] . maximizeWithPadding 0
  hiddenMod = renamed [CutWordsLeft 1] . hiddenWindows
  allLays   = bspace ||| tall ||| two ||| full ||| accordion

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
                  , ("Tall",                   "Tall")
                  , ("Two Column",             "2Col")
                  , ("Full Screen",            "Full")
                  , ("Full Float",             "Float")
                  , ("Binary Space Partition", "BSP")
                  ]
