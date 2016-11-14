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
       , names
       , asKey
       , viewPrevWS
       ) where

--------------------------------------------------------------------------------
import XMonad.Actions.DynamicProjects
import Control.Monad (unless)
import XMonad
import qualified XMonad.StackSet as StackSet

--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "scratch"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "tasks"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "urxvtc -e ncmpcpp"
                                           spawn "chromium --app=http://rememberthemilk.com"
            }

  , Project { projectName      = "clocks"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "urxvtc -name BigTerm -e tty-clock -c -C 4 -f '%b. %d, %Y'"
                                           spawn "urxvtc -name BigTerm -e env TZ=America/New_York tty-clock -C 0 -c -f 'South Carolina'"
            }

  , Project { projectName      = "browsers"
            , projectDirectory = "~/download"
            , projectStartHook = Just $ do spawn "conkeror"
                                           spawn "chromium"
            }

  , Project { projectName      = "mail"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "e -cs irc"
                                           spawn "e -cs gnus"
            }

  , Project { projectName      = "xmonad"
            , projectDirectory = "~/core/xmonad"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "training"
            , projectDirectory = "~/training/courses"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "rfa"
            , projectDirectory = "~/develop/rfa"
            , projectStartHook = Nothing
            }
  ]

--------------------------------------------------------------------------------
-- | Names of my workspaces.
names :: [WorkspaceId]
names = ["scratch", "tasks", "clocks", "browsers", "mail"]

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
  let hs = StackSet.hidden ws
  unless (null hs) (windows . StackSet.view . StackSet.tag $ head hs)
