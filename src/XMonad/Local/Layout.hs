{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS -fno-warn-missing-signatures #-}

-- |
--
-- Copyright:
--   This file is part of the package xmonadrc. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/xmonadrc
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the
--   terms contained in the LICENSE file.
--
-- License: BSD-3-Clause
--
-- Layout configuration and hook.
module XMonad.Local.Layout
  ( layoutHook,
    selectLayoutByName,
    toggleLayout,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import XMonad hiding (float, layoutHook, (|||))
import XMonad.Layout.Column (Column (..))
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.IfMax (ifMax)
import XMonad.Layout.LayoutBuilder
  ( LayoutB,
    SubBox (..),
    SubMeasure (..),
    layoutAll,
    layoutN,
    layoutP,
    relBox,
  )
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize (maximizeWithPadding)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.OneBig (OneBig (..))
import XMonad.Layout.Renamed (Rename (..), renamed)
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.TwoPane (TwoPane (..))
import XMonad.Layout.ZoomRow (zoomRow)
import XMonad.Local.Layout.Columns
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Prompt
import qualified XMonad.StackSet as Stack
import XMonad.Util.ExtensibleState as XState
import XMonad.Util.WindowProperties (Property (..))

-- | XMonad layout hook.  No type signature because it's freaking
-- nasty and I can't come up with a way to make it generic.
layoutHook =
  renamed [CutWordsLeft 2]
    $ minimize
    $ maximizeWithPadding 100 allLays
  where
    uniformBorder n = Border n n n n
    spacing = spacingRaw False (uniformBorder 0) True (uniformBorder 10) True
    full = noBorders Full
    big = spacing $ OneBig (3 / 4) (3 / 4)
    tall = spacing $ mkCols 0 1 2 (3 / 5) (1 / 100) RightToLeft
    twoPane = spacing $ TwoPane (1 / 100) (1 / 2)
    focusTag = spacing $ only (Tagged "focus")
    grid = ifMax 3 tall (spacing Grid)
    pgrid = pad (1 / 8) grid
    ten80 = centered 2560 (1924, 1084) -- Account for border width
    cgrid = layoutAll (relBox (1 / 8) (1 / 8) (7 / 8) (7 / 8)) grid
    single = noBorders (pad (1 / 4) Simplest)
    auto = ifMax 1 (noBorders cgrid) $ ifMax 2 twoPane mcols
    padded = ifMax 1 single (pad (1 / 8) tall)
    mcols = spacing $ mkCols 1 1 3 (3 / 8) (1 / 100) RightToLeft
    -- Layout modifier that places the requested padding around the
    -- given layout.
    pad ::
      (Read a, Eq a, LayoutClass l1 a) =>
      Rational ->
      l1 a ->
      LayoutB l1 Full () a
    pad offset = layoutAll (relBox offset (1 / 30) (1 - offset) (29 / 30))
    -- A layout where windows you want to focus on are specified using
    -- @WindowProperties@.  Windows matching the given properties will
    -- be placed into the main layout.  Other windows are pushed to
    -- the bottom of the screen into a single row.
    only prop =
      let topBox = relBox 0 0 1 (7 / 8)
          botBox = relBox 0 (7 / 8) 1 1
       in layoutP prop topBox Nothing mcols $
            layoutAll botBox zoomRow
    -- Center the master window horizontally, locked to the given
    -- width and height, display all other windows below in a grid.
    centered x (w, h) =
      let centerBox = SubBox (Abs ((x - w) `div` 2)) (Abs 10) (Abs w) (Abs h)
          bottomBox = SubBox (Rel 0) (Abs (h + 10)) (Rel 1) (Rel 1)
       in layoutN 1 centerBox Nothing Grid $
            layoutAll bottomBox (spacing zoomRow)
    chat =
      let leftBox = relBox 0 0 (2 / 3) 1
          rightBox = relBox (2 / 3) 0 1 1
          prop = ClassName "Emacs"
       in layoutP prop leftBox Nothing grid $
            layoutAll rightBox (spacing $ Column 1)
    allLays =
      renamed [Replace "Auto"] auto
        ||| renamed [Replace "1080p"] ten80
        ||| renamed [Replace "2P"] twoPane
        ||| renamed [Replace "Big"] big
        ||| renamed [Replace "Chat"] chat
        ||| renamed [Replace "Focus"] focusTag
        ||| renamed [Replace "Full"] full
        ||| renamed [Replace "Grid"] grid
        ||| renamed [Replace "MultiCols"] mcols
        ||| renamed [Replace "Padded"] padded
        ||| renamed [Replace "PGrid"] pgrid
        ||| renamed [Replace "Single"] single
        ||| renamed [Replace "Tall"] tall

-- | A data type for the @XPrompt@ class.
data LayoutByName = LayoutByName

instance XPrompt LayoutByName where
  showXPrompt LayoutByName = "Layout: "

-- | Use @Prompt@ to choose a layout.
selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf =
  mkXPrompt LayoutByName conf' (aListCompFunc conf' layoutNames) go
  where
    go :: String -> X ()
    go selected =
      case lookup selected layoutNames of
        Nothing -> return ()
        Just name -> sendMessage (JumpToLayout name)
    conf' :: XPConfig
    conf' = conf {alwaysHighlight = True}
    layoutNames :: [(String, String)]
    layoutNames =
      [ ("Auto", "Auto"),
        ("1080p", "1080p"),
        ("Big", "Big"),
        ("Chat", "Chat"),
        ("Focus", "Focus"),
        ("Full", "Full"),
        ("Grid", "Grid"),
        ("Padded Grid", "PGrid"),
        ("Padded", "Padded"),
        ("Single", "Single"),
        ("Tall", "Tall"),
        ("Two Pane (2P)", "2P")
      ]

-- | Keep track of layouts when jumping with 'toggleLayout'.
newtype LayoutHistory = LayoutHistory
  {runLayoutHistory :: Map String String}
  deriving (Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory Map.empty

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
