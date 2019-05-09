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
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.ComboP (PartitionWins(..), combineTwoP)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.GridVariants (SplitGrid(..))
import qualified XMonad.Layout.GridVariants as Grid
import XMonad.Layout.IfMax (ifMax)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master (mastered)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Reflect (reflectVert)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Prompt
import XMonad.Util.Types (Direction2D(..))
import XMonad.Util.WindowProperties (Property(..))

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook =
    toggleLayouts
      (noBorders fullscreen)
      allLays
  where
    uniformGaps n = [(U, n), (D, n), (L, n), (R, n)] :: [(Direction2D,Int)]
    uniformBorder n = Border n n n n
    spacing = spacingRaw False (uniformBorder 0) False (uniformBorder 10) True

    fullscreen = gaps (uniformGaps 60) Full
    threeCols  = spacing $ ThreeColMid 1 (1/100) (3/8)
    twoCols    = spacing $ mastered (1/100) (1/2) Accordion
    twoPane    = spacing $ TwoPane (1/100) (1/2)
    tall       = spacing $ ResizableTall 1 (1/100) (3/5) []
    focusTag   = spacing $ only (Tagged "focus")
    grid       = spacing $ SplitGrid Grid.L 2 2 (2/3) (1/2) 1
    auto       = ifMax 1 centFull $ ifMax 2 twoPane threeCols

    -- One window, centered on the screen.
    centFull  = spacingRaw False (Border 20 20 400 400)
                           True  (Border 0 0 0 0) False Full

    -- A layout where windows you want to focus on are specified using
    -- @WindowProperties@.  Windows matching the given properties will
    -- be placed into the main layout.  Other windows are pushed to
    -- the top of the screen in a small @Accordion@.
    only = combineTwoP (reflectVert $ Mirror $ TwoPane 0 (9/10))
                       twoCols (Mirror Accordion)

    -- All layouts put together.
    allLays =
      renamed [Replace "Auto"]  auto     |||
      renamed [Replace "Tall"]  tall      |||
      renamed [Replace "3C"]    threeCols |||
      renamed [Replace "2C"]    twoCols   |||
      renamed [Replace "2P"]    twoPane   |||
      renamed [Replace "Focus"] focusTag  |||
      renamed [Replace "Grid"]  grid      |||
      renamed [Replace "Full"]  fullscreen

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
    go selected =
      case lookup selected layoutNames of
        Nothing   -> return ()
        Just name -> do sendMessage (JumpToLayout name)
                        sendMessage PartitionWins

    layoutNames :: [(String, String)]
    layoutNames =
      [ ("Auto",               "Auto")
      , ("Tall",               "Tall")
      , ("Three Columns (3C)", "3C")
      , ("Two Columns (2C)",   "2C")
      , ("Two Pane (2P)",      "2P")
      , ("Full",               "Full")
      , ("Focus",              "Focus")
      , ("Grid",               "Grid")
      ]
