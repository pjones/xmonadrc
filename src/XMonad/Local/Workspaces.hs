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
       ( projects
       , terminal
       , names
       , scratchPads
       , asKey
       , viewPrevWS
       ) where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import XMonad hiding (terminal)
import XMonad.Actions.DynamicProjects
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as StackSet
import XMonad.Util.NamedScratchpad

--------------------------------------------------------------------------------
-- | The command to run to start a terminal.
terminal :: String
terminal = "konsole --notransparency --workdir $(pwd)"

--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "scratch"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "e -c"
                spawn terminal
                spawn terminal
            }

  , Project { projectName      = "browsers"
            , projectDirectory = "~/download"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "Tall")
                spawn "firefox"
            }

  , Project { projectName      = "monitoring"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "Tall")
                setScreenSpacing $ Border 40 40 40 40
                spawn "chromium --app='https://stats.devalot.com/d/trW0cTIZz/server-health?orgId=1&refresh=10s&kiosk'"
                spawn "chromium --app='http://hass.pmade.com:8123/lovelace/0'"
            }

  , Project { projectName      = "chat"
            , projectDirectory = "~/download"
            , projectStartHook = Just $
                sendMessage (JumpToLayout "Chat")
            }

  , Project { projectName      = "mail"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "Mail")
                spawn "e -cs mail"
            }

  , Project { projectName      = "rc"
            , projectDirectory = "~/src/rc"
            , projectStartHook = Just $ do
                spawn "e -cs rc"
                spawn terminal
            }

  , Project { projectName      = "rfa"
            , projectDirectory = "~/src/rfa"
            , projectStartHook = Just $ do
                spawn "e -cs rfa"
                spawn terminal
            }
  ]

--------------------------------------------------------------------------------
-- | Names of my workspaces.
names :: [WorkspaceId]
names = ["scratch", "browsers", "music", "mail", "chat"]

--------------------------------------------------------------------------------
scratchPads :: NamedScratchpads
scratchPads =
  [ NS { name  = "emacs"
       , cmd   = "e -c -- -F '((name . \"scratch\"))'"
       , query = className =? "Emacs" <&&> appName =? "scratch"
       , hook  = floatOnRight
       }
  ]
  where
    floatOnRight = customFloating $
      StackSet.RationalRect (2/3) (1/10) (1/3) (8/10)

--------------------------------------------------------------------------------
-- | Helper function to translate workspace names into key names for
-- using in @EZConfig@ key bindings.
asKey :: String -> String
asKey ('F':xs) = "<F" ++ xs ++ ">" -- Function key (i.e. "F1" -> "<F1>")
asKey x        = x                 -- Any other key.

--------------------------------------------------------------------------------
-- | Toggle between the current and previous workspaces.
viewPrevWS :: X ()
viewPrevWS = do
  ws <- gets windowset
  let hs = filter (\w -> StackSet.tag w /= "NSP") $ StackSet.hidden ws
  unless (null hs) (windows . StackSet.view . StackSet.tag $ head hs)
