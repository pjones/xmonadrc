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
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.TwoPane (TwoPane(..))

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook = avoidStruts {- $ windowNavigation -} layouts

--------------------------------------------------------------------------------
-- | All of the layouts and layout modifiers that I use.  See the
-- documentation for @layoutHook@ above for information about the type
-- signature.
layouts =  floatF12 maxToggle where
  tall      = renamed [Replace "Tall"]  $ ResizableTall 1 (1.5/100) (3/5) []
  rtall     = renamed [Replace "RTall"] $ reflectHoriz tall
  mtall     = renamed [Replace "MTall"] $ Mirror tall
  two       = renamed [Replace "2Col"]  $ TwoPane (3/100) (3/5)
  three     = renamed [Replace "3Col"]  $ ThreeColMid 1 (3/100) (1/2)
  full      = renamed [Replace "Full"]  $ noBorders Full
  float     = renamed [Replace "Float"] simplestFloat
  bspace    = renamed [Replace "BSP"] emptyBSP
  floatF12  = onWorkspace "F12" float
  maxToggle = renamed [CutWordsLeft 1] $ maximize toggle
  toggle    = tall  ||| rtall ||| mtall |||
              three ||| two   ||| full  ||| bspace

--------------------------------------------------------------------------------
-- | Use GridSelect to choose a layout.
selectLayoutByName :: GSConfig String -> X ()
selectLayoutByName conf = do
  selected <- gridselect conf layoutNames

  case selected of
    Just name -> sendMessage (JumpToLayout name)
    Nothing   -> return ()

  where
    layoutNames = [ ("Tall",                   "Tall")
                  , ("Reflected Tall",         "RTall")
                  , ("Mirrored Tall",          "MTall")
                  , ("Two Column",             "2Col")
                  , ("Three Column",           "3Col")
                  , ("Full Screen",            "Full")
                  , ("Full Float",             "Float")
                  , ("Binary Space Partition", "BSP")
                  ]
