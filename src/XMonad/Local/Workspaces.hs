--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------

-- | Workspace configuration and utilities.
module XMonad.Local.Workspaces
  ( projects,
    terminal,
    names,
    scratchPads,
    asKey,
    viewPrevWS,
  )
where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import XMonad hiding (terminal)
import XMonad.Actions.DynamicProjects
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import qualified XMonad.StackSet as StackSet
import XMonad.Util.NamedScratchpad

--------------------------------------------------------------------------------

-- | The command to run to start a terminal.
terminal :: String
terminal = "eterm"

-- | The full-screen browser to use:
browserFull :: String
browserFull = "vimb -C 'set status-bar=false' "

--------------------------------------------------------------------------------
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
          spawn "vimb https://calendar.google.com/calendar/"
          spawn "vimb"
      },
    Project
      { projectName = "monitoring",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn (browserFull <> "'https://stats.devalot.com/d/fkNz2pRMz/system-health?orgId=1&from=now-1h&to=now&refresh=30s&kiosk&var-node=kilgrave&var-node=medusa'")
          spawn (browserFull <> "'https://stats.devalot.com/d/fkNz2pRMz/system-health?orgId=1&from=now-1h&to=now&refresh=30s&kiosk&var-node=moriarty&var-node=ursula'")
          spawn (browserFull <> "'https://stats.devalot.com/d/9H98YpRMk/mail?orgId=1&refresh=1m&kiosk'")
          spawn (browserFull <> "'https://stats.devalot.com/d/UJ0W9oRGk/headquarters?openVizPicker&orgId=1&from=now-3h&to=now&refresh=30s&kiosk'")
      },
    Project
      { projectName = "chat",
        projectDirectory = "~/download",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Chat")
          spawn (browserFull <> "https://messages.google.com/web/conversations")
          spawn (browserFull <> "https://chat.rfa.sc.gov/login")
      },
    Project
      { projectName = "mail",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "kmail"
      },
    Project
      { projectName = "rc",
        projectDirectory = "~/src/rc",
        projectStartHook = Nothing
      },
    Project
      { projectName = "music",
        projectDirectory = "~/documents/music",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Tall")
          spawn "spotify"
          spawn "pavucontrol"
      },
    Project
      { projectName = "rfa",
        projectDirectory = "~/src/rfa",
        projectStartHook = Nothing
      },
    Project
      { projectName = "sthenauth",
        projectDirectory = "~/src/sthenauth",
        projectStartHook = Nothing
      },
    Project
      { projectName = "iolaus",
        projectDirectory = "~/src/haskell/iolaus",
        projectStartHook = Nothing
      },
    Project
      { projectName = "rip",
        projectDirectory = "~/documents/ripping",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "Tall")
          spawn "ghb"
      },
    Project
      { projectName = "meetings",
        projectDirectory = "~/download",
        projectStartHook = Just $ do
          sendMessage (JumpToLayout "1080p")
          spawn "chromium"
      }
  ]

--------------------------------------------------------------------------------

-- | Names of my workspaces.
names :: [WorkspaceId]
names = ["scratch", "browsers", "music", "mail", "chat"]

--------------------------------------------------------------------------------
scratchPads :: NamedScratchpads
scratchPads =
  [ NS
      { name = "emacs",
        cmd = "e -s notes -c -- -F '((name . \"notes\"))'",
        query = className =? "Emacs" <&&> appName =? "notes",
        hook = floatOnRight
      },
    NS
      { name = "ffdoc",
        cmd = "tabbed -ckn ffdoc vimb -e",
        query = appName =? "ffdoc",
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

--------------------------------------------------------------------------------

-- | Helper function to translate workspace names into key names for
-- using in @EZConfig@ key bindings.
asKey :: String -> String
asKey ('F' : xs) = "<F" ++ xs ++ ">" -- Function key (i.e. "F1" -> "<F1>")
asKey x = x -- Any other key.

--------------------------------------------------------------------------------

-- | Toggle between the current and previous workspaces.
viewPrevWS :: X ()
viewPrevWS = do
  ws <- gets windowset
  let hs = filter (\w -> StackSet.tag w /= "NSP") $ StackSet.hidden ws
  unless (null hs) (windows . StackSet.view . StackSet.tag $ head hs)
