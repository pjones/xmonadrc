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
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.ComboP (PartitionWins(..), combineTwoP)
import XMonad.Layout.Cross (Cross(..))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.GridVariants (SplitGrid(..))
import qualified XMonad.Layout.GridVariants as Grid
import XMonad.Layout.IfMax (ifMax)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier (magnifiercz)
import XMonad.Layout.Master (mastered)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.OneBig (OneBig(..))
import XMonad.Layout.Reflect (reflectHoriz, reflectVert)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.ZoomRow (zoomRow)
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Prompt
import XMonad.Util.WindowProperties (Property(..))

import XMonad.Layout.LayoutBuilder
  ( SubBox(..)
  , SubMeasure(..)
  , layoutN
  , layoutP
  , layoutAll
  , relBox
  )

--------------------------------------------------------------------------------
-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook = toggleLayouts oneCol allLays
  where
    uniformBorder n = Border n n n n
    spacing  = spacingRaw False (uniformBorder 0) False (uniformBorder 10) True
    spacing' = spacingRaw False (uniformBorder 0) False (uniformBorder 2)  True

    full       = noBorders Full
    cmaster    = centerMaster grid
    big        = spacing $ OneBig (3/4) (3/4)
    oneCol     = spacing' $ magnifiercz 0.9 Accordion
    threeCols  = spacing $ reflectHoriz $ ThreeColMid 1 (1/100) (3/8)
    twoCols    = spacing $ mastered (1/100) (1/2) Accordion
    twoPane    = spacing $ TwoPane (1/100) (1/2)
    tall       = spacing $ ResizableTall 1 (1/100) (3/5) []
    focusTag   = spacing $ only (Tagged "focus")
    grid       = spacing Grid
    sgrid      = spacing $ SplitGrid Grid.L 2 2 (2/3) (1/2) 1
    cross      = spacing $ Cross (4/5) (1/100)
    ten80      = centered 2560 (1930, 1090) -- Account for border width
    cgrid      = layoutAll (relBox (1/8) (1/8) (7/8) (7/8)) grid
    auto       = ifMax 1 (noBorders cgrid) $ ifMax 2 twoPane threeCols

    -- A layout where windows you want to focus on are specified using
    -- @WindowProperties@.  Windows matching the given properties will
    -- be placed into the main layout.  Other windows are pushed to
    -- the bottom of the screen into a single row.
    only prop =
      let topBox = relBox 0 0 1 (7/8)
          botBox = relBox 0 (7/8) 1 1
      in layoutP prop topBox Nothing threeCols $
           layoutAll botBox zoomRow

    -- Center the master window horizontally, locked to the given
    -- width and height, display all other windows below in a grid.
    centered x (w, h) =
      let centerBox = SubBox (Abs ((x - w) `div` 2)) (Abs 10) (Abs w) (Abs h)
          bottomBox = SubBox (Rel 0) (Abs (h + 10)) (Rel 1) (Rel 1)
      in layoutN 1 centerBox Nothing Grid $
           layoutAll bottomBox (spacing zoomRow)

    allLays =
      renamed [Replace "Auto"]     auto      |||
      renamed [Replace "Tall"]     tall      |||
      renamed [Replace "3C"]       threeCols |||
      renamed [Replace "2C"]       twoCols   |||
      renamed [Replace "2P"]       twoPane   |||
      renamed [Replace "Focus"]    focusTag  |||
      renamed [Replace "Cross"]    cross     |||
      renamed [Replace "Grid"]     grid      |||
      renamed [Replace "SGrid"]    sgrid     |||
      renamed [Replace "Big"]      big       |||
      renamed [Replace "1080p"]    ten80     |||
      renamed [Replace "Centered"] cmaster   |||
      renamed [Replace "Full"]     full

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
      , ("1080p",              "1080p")
      , ("Big",                "Big")
      , ("Centered",           "Centered")
      , ("Cross",              "Cross")
      , ("Focus",              "Focus")
      , ("Full",               "Full")
      , ("Grid",               "Grid")
      , ("Split Grid",         "SGrid")
      , ("Tall",               "Tall")
      , ("Three Columns (3C)", "3C")
      , ("Two Columns (2C)",   "2C")
      , ("Two Pane (2P)",      "2P")
      ]
