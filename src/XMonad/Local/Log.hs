--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Utility functions for the XMonad @logHook@.
module XMonad.Local.Log (logHook) where

--------------------------------------------------------------------------------
import System.IO (Handle, hPutStrLn)
import XMonad hiding (logHook)
import XMonad.Hooks.DynamicLog hiding (xmobar)
import XMonad.Hooks.FadeInactive (isUnfocusedOnCurrentWS)
import XMonad.Hooks.FadeWindows
import XMonad.Local.Workspaces (hiddenName)

--------------------------------------------------------------------------------
-- | XMonad @logHook@.
logHook :: Handle -> X ()
logHook h = do
  fadeWindowsLogHook fadeHook
  dynamicLogWithPP $ statusBarPrinter h

--------------------------------------------------------------------------------
-- | Fade hooks compose from right to left, or in my case from bottom
-- to top.  The first match wins (again, moving from bottom to top).
--
-- Specific notes:
--
--   * Blender: There's an issue between Blender and xcompmgr that
--     I've found which causes Blender to have transparency no matter
--     what.  Using @blender-softwaregl@ seems to fix that though.
fadeHook :: FadeHook
fadeHook = composeAll
  [ transparency 0.02      --  Default
  , isUnfocusedOnCurrentWS --> transparency 0.1
  , isFloating             --> opaque
  , className =? "feh"     --> opaque
  , className =? "Vlc"     --> opaque
  , className =? "Blender" --> opaque -- See note above.
  ]

--------------------------------------------------------------------------------
statusBarPrinter :: Handle -> PP
statusBarPrinter output = defaultPP
  { ppCurrent         = xmobarColor "#88b324" "" . wrap "[" "]"
  , ppVisible         = wrap "(" ")"
  , ppHidden          = id
  , ppHiddenNoWindows = xmobarColor "#3c3c3c" "" . hiddenName
  , ppUrgent          = xmobarColor "#1c1c1c" "#d33682" . wrap "<" ">"
  , ppSep             = xmobarColor "#5c5c5c" "" " | "
  , ppWsSep           = " "
  , ppTitle           = const ""
  , ppOutput          = hPutStrLn output
  }
