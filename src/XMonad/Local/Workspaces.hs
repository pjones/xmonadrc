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
                spawn "urxvtc"
            }

  , Project { projectName      = "clocks"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "BSP")
                spawn "urxvtc -e ncmpcpp"
                spawn "urxvtc -name BigTerm -e env TZ=America/New_York tty-clock -C 0 -c -f 'South Carolina'"
                spawn "urxvtc -name BigTerm -e tty-clock -c -C 4 -f '%b. %d, %Y'"
            }

  , Project { projectName      = "browsers"
            , projectDirectory = "~/download"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "BSP")
                spawn "chromium"
                spawn "chromium"
            }

  , Project { projectName      = "mail"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                sendMessage (JumpToLayout "3C")
                spawn "e -cs irc -- -F '((name . \"irc\"))'"
                spawn "e -cs gnus"
            }

  , Project { projectName      = "emacs"
            , projectDirectory = "~/core/emacs"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "rip"
            , projectDirectory = "~/documents/ripping"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "xmonad"
            , projectDirectory = "~/core/xmonadrc"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "rfa"
            , projectDirectory = "~/develop/rfa"
            , projectStartHook = Nothing
            }

    -- Classes I teach:
  , Project { projectName      = "training"
            , projectDirectory = "~/training/courses"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "advjs"
            , projectDirectory = "~/training/courses/src/javascript/advjs"
            , projectStartHook = Just workshopHook
            }

  , Project { projectName      = "corejs"
            , projectDirectory = "~/training/courses/src/javascript/corejs"
            , projectStartHook = Just workshopHook
            }

  , Project { projectName      = "intcss"
            , projectDirectory = "~/training/courses/src/css/intcss"
            , projectStartHook = Just workshopHook
            }
  ]
  where
    workshopHook = do
      sendMessage (JumpToLayout "Projector")
      spawn "e -c"
      spawn "urxvtc -name BigTerm -e clockdown"
      spawn "zathura"



--------------------------------------------------------------------------------
-- | Names of my workspaces.
names :: [WorkspaceId]
names = ["scratch", "tasks", "clocks", "browsers", "mail"]

--------------------------------------------------------------------------------
scratchPads :: NamedScratchpads
scratchPads =
  [ NS { name  = "calc"
       , cmd   = "e -c -s calc -e '(full-calc)' -- -F '((name . \"calculator\"))'"
       , query = appName =? "calculator"
       , hook  = floatOnRight
       }
  , NS { name = "pass"
       , cmd  = "e -c -s pass -e '(passmm-list-passwords)' -- -F '((name . \"passwords\"))'"
       , query = appName =? "passwords"
       , hook  = floatOnRight
       }
  , NS { name  = "todoist"
       , cmd   = "chromium --app=https://todoist.com/app"
       , query = appName =? "todoist.com__app"
       , hook  = floatOnRight
       }
  ]
  where
    floatOnRight = customFloating $ StackSet.RationalRect (2/3) 0 (1/3) 0.98

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
