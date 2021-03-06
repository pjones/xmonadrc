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
-- Workspace configuration and utilities.
module XMonad.Local.Workspaces
  ( projects,
    terminal,
    names,
    scratchPads,
    asKey,
    viewPrevWS,
  )
where

import Control.Monad (unless)
import Text.Printf (printf)
import XMonad hiding (terminal)
import XMonad.Actions.DynamicProjects
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import XMonad.Local.Prompt (WebApp (..), webAppCommand)
import qualified XMonad.StackSet as StackSet
import XMonad.Util.NamedScratchpad

-- | The command to run to start a terminal.
terminal :: String
terminal = "konsole"

projects :: [Project]
projects =
  [ Project
      { projectName = "scratch",
        projectDirectory = "~/",
        projectStartHook = Nothing
      },
    Project
      { projectName = "browsers",
        projectDirectory = "~/download",
        projectStartHook = Just $ do
          spawn "browser https://calendar.google.com/calendar/"
          spawn "browser"
      },
    Project
      { projectName = "chat",
        projectDirectory = "~/download",
        projectStartHook = Just $ do
          spawn (webAppCommand GoogleMessages)
          spawn (webAppCommand Mattermost)
          spawn "signal-desktop"
      },
    Project
      { projectName = "mail",
        projectDirectory = "~/download",
        projectStartHook = Just $ do
          spawn (webAppCommand Outlook)
          spawn "e -cs mail"
      },
    Project
      { projectName = "rc",
        projectDirectory = "~/src/rc",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Padded")
          spawn "e -cs rc"
      },
    Project
      { projectName = "rfa",
        projectDirectory = "~/src/rfa",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Padded")
          spawn "e -cs rfa"
      },
    Project
      { projectName = "nix-hs",
        projectDirectory = "~/src/haskell/nix-hs",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Padded")
          spawn "e -cs nix-hs"
      },
    Project
      { projectName = "edify",
        projectDirectory = "~/src/haskell/edify",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Padded")
          spawn "e -cs edify"
      },
    Project
      { projectName = "rip",
        projectDirectory = "~/documents/ripping",
        projectStartHook = Just $ do
          spawn "ghb"
      },
    Project
      { projectName = "music",
        projectDirectory = "~/documents/music",
        projectStartHook = Just $ do
          spawn "spotify"
          spawn "cantata"
      },
    Project
      { projectName = "social",
        projectDirectory = "~/download",
        projectStartHook = Just $ do
          spawn (webAppCommand Tweetdeck)
          spawn "zulip"
      },
    Project
      { projectName = "monitoring",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          let browserFull = printf "sleep 1 && browser --app='%s'"
          spawn (browserFull "https://stats.devalot.com/d/fkNz2pRMz/system-health?orgId=1&from=now-1h&to=now&refresh=30s&kiosk&var-node=kilgrave&var-node=medusa")
          spawn (browserFull "https://stats.devalot.com/d/fkNz2pRMz/system-health?orgId=1&from=now-1h&to=now&refresh=30s&kiosk&var-node=moriarty&var-node=ursula")
          spawn (browserFull "https://stats.devalot.com/d/9H98YpRMk/mail?orgId=1&refresh=1m&kiosk")
          spawn (browserFull "https://stats.devalot.com/d/UJ0W9oRGk/headquarters?openVizPicker&orgId=1&from=now-3h&to=now&refresh=30s&kiosk")
      }
  ]

-- | Names of my workspaces.
names :: [WorkspaceId]
names = map projectName projects

scratchPads :: NamedScratchpads
scratchPads =
  [ NS
      { name = "emacs",
        cmd = "e -ncs notes",
        query =
          className =? "Emacs"
            <&&> ( stringProperty "WM_WINDOW_ROLE" =? "notes"
                     <||> appName =? "notes"
                 ),
        hook = floatOnRight
      },
    NS
      { name = "browser",
        cmd = "browser-sidebar",
        query = className =? "browser-sidebar",
        hook = floatOnLeft
      }
  ]
  where
    floatOnRight =
      customFloating $
        StackSet.RationalRect (1 / 2) (1 / 10) (1 / 2) (8 / 10)
    floatOnLeft =
      customFloating $
        StackSet.RationalRect 0 (1 / 10) (1 / 2) (8 / 10)

-- | Helper function to translate workspace names into key names for
-- using in @EZConfig@ key bindings.
asKey :: String -> String
asKey ('F' : xs) = "<F" ++ xs ++ ">" -- Function key (i.e. "F1" -> "<F1>")
asKey x = x -- Any other key.

-- | Toggle between the current and previous workspaces.
viewPrevWS :: X ()
viewPrevWS = do
  ws <- gets windowset
  let hs = filter (\w -> StackSet.tag w /= "NSP") $ StackSet.hidden ws
  unless (null hs) (windows . StackSet.view . StackSet.tag $ head hs)
