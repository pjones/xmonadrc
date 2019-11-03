{-# LANGUAGE DeriveDataTypeable          #-}
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
module XMonad.Local.Layout
  ( layoutHook
  , selectLayoutByName
  , toggleLayout
  ) where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import XMonad hiding ((|||), layoutHook, float)
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.IfMax (ifMax)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Master (mastered)
import XMonad.Layout.Maximize (maximizeWithPadding)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.OneBig (OneBig(..))
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.ZoomRow (zoomRow)
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Prompt
import qualified XMonad.StackSet as Stack
import XMonad.Util.ExtensibleState as XState
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
layoutHook =
    renamed [CutWordsLeft 1] $
      maximizeWithPadding 100 allLays
  where
    uniformBorder n = Border n n n n
    spacing = spacingRaw False (uniformBorder 0) True (uniformBorder 10) True

    full       = noBorders Full
    big        = spacing $ OneBig (3/4) (3/4)
    threeCols  = spacing $ reflectHoriz $ ThreeColMid 1 (1/100) (3/8)
    twoCols    = spacing $ mastered (1/100) (1/2) (Mirror zoomRow)
    twoPane    = spacing $ TwoPane (1/100) (1/2)
    tall       = spacing $ ResizableTall 1 (1/100) (3/5) []
    focusTag   = spacing $ only (Tagged "focus")
    grid       = spacing Grid
    ten80      = centered 2560 (1924, 1084) -- Account for border width
    cgrid      = layoutAll (relBox (1/8) (1/8)  (7/8) (7/8))   grid
    small      = layoutAll (relBox (1/4) (1/8)  (3/4) (7/8))   twoPane
    single     = layoutAll (relBox (1/4) (1/30) (3/4) (29/30)) Simplest
    auto       = ifMax 1 (noBorders cgrid) $ ifMax 2 twoPane threeCols
    mail       = ifMax 1 (noBorders small) $ ifMax 2 small threeCols

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

    chat =
      let leftBox  = relBox 0 0 (2/3) 1
          rightBox = relBox (2/3) 0 1 1
          prop     = ClassName "Emacs"
      in layoutP prop leftBox Nothing grid $
           layoutAll rightBox grid

    -- Firefox on the left, other windows in a tall config on the right.
    dev =
      let leftBox  = relBox 0 0 (1/3) 1
          rightBox = relBox (1/3) 0 1 1
          prop     = ClassName "Firefox"
      in layoutP prop leftBox Nothing grid $
           layoutAll rightBox tall

    allLays =
      renamed [Replace "Auto"]      auto      |||
      renamed [Replace "1080p"]     ten80     |||
      renamed [Replace "2C"]        twoCols   |||
      renamed [Replace "2P"]        twoPane   |||
      renamed [Replace "3C"]        threeCols |||
      renamed [Replace "Big"]       big       |||
      renamed [Replace "Chat"]      chat      |||
      renamed [Replace "Dev"]       dev       |||
      renamed [Replace "Focus"]     focusTag  |||
      renamed [Replace "Grid"]      grid      |||
      renamed [Replace "Mail"]      mail      |||
      renamed [Replace "Single"]    single    |||
      renamed [Replace "Tall"]      tall      |||
      renamed [Replace "Full"]      full

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
        Just name -> sendMessage (JumpToLayout name)

    layoutNames :: [(String, String)]
    layoutNames =
      [ ("Auto",               "Auto")
      , ("1080p",              "1080p")
      , ("Big",                "Big")
      , ("Chat",               "Chat")
      , ("Dev",                "Dev")
      , ("Focus",              "Focus")
      , ("Full",               "Full")
      , ("Grid",               "Grid")
      , ("Mail",               "Mail")
      , ("Single",             "Single")
      , ("Tall",               "Tall")
      , ("Three Columns (3C)", "3C")
      , ("Two Columns (2C)",   "2C")
      , ("Two Pane (2P)",      "2P")
      ]

--------------------------------------------------------------------------------
-- | Keep track of layouts when jumping with 'toggleLayout'.
newtype LayoutHistory = LayoutHistory
  { runLayoutHistory :: Map String String }
  deriving (Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory Map.empty

--------------------------------------------------------------------------------
-- | Toggle between the current layout and the one given as an argument.
toggleLayout :: String -> X ()
toggleLayout name = do
  winset <- XMonad.gets windowset

  let ws = Stack.workspace . Stack.current $ winset
      wn = Stack.tag ws
      ld = description . Stack.layout $ ws

  if name == ld
    then restoreLayout wn
    else rememberAndGo wn ld

  where
    -- Restore the previous workspace.
    restoreLayout :: String -> X ()
    restoreLayout ws = do
      history <- runLayoutHistory <$> XState.get
      let ld = fromMaybe "Auto" (Map.lookup ws history)
      sendMessage (JumpToLayout ld)

    -- Remember the current workspace and jump to the requested one.
    rememberAndGo :: String -> String -> X ()
    rememberAndGo ws current = do
      history <- runLayoutHistory <$> XState.get
      XState.put (LayoutHistory $ Map.insert ws current history)
      sendMessage (JumpToLayout name)
