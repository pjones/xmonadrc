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
       , scratchPads
       , asKey
       , viewPrevWS
       ) where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import qualified XMonad.StackSet as StackSet
import XMonad.Util.NamedScratchpad

--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "scratch"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "e -c"
                spawn "konsole --workdir $(pwd)"
                spawn "konsole --workdir $(pwd)"
            }

  , Project { projectName      = "browsers"
            , projectDirectory = "~/download"
            , projectStartHook = Just $ spawn "firefox"
            }

  , Project { projectName      = "chat"
            , projectDirectory = "~/download"
            , projectStartHook = Just $
                sendMessage (JumpToLayout "Chat")
            }

  , Project { projectName      = "mail"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "Auto Tall")
                spawn "e -cs mail"
            }

  , Project { projectName      = "rc"
            , projectDirectory = "~/src/rc"
            , projectStartHook = Just $ do
                spawn "e -cs rc"
                spawn "konsole --workdir $(pwd)"
            }

  , Project { projectName      = "rfa"
            , projectDirectory = "~/src/rfa"
            , projectStartHook = Just $ do
                spawn "e -cs rfa"
                spawn "konsole --workdir $(pwd)"
            }

  , Project { projectName      = "mint"
            , projectDirectory = "~/src/mint"
            , projectStartHook = Just $ do
                spawn "e -cs mint"
                spawn "konsole --workdir $(pwd)"
            }

  ]

--------------------------------------------------------------------------------
-- | Names of my workspaces.
names :: [WorkspaceId]
names = ["scratch", "browsers", "agenda", "music", "mail", "chat"]

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
