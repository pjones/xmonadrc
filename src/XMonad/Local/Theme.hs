--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Color theme.
module XMonad.Local.Theme (xmonadColors, decoTheme) where

--------------------------------------------------------------------------------
import XMonad
import XMonad.Layout.SimpleDecoration

--------------------------------------------------------------------------------
windowBorderColor :: String
windowBorderColor = "#6c71c4"

--------------------------------------------------------------------------------
windowDecoColor :: String
windowDecoColor = "#9396c4"

--------------------------------------------------------------------------------
xmonadColors :: XConfig a -> XConfig a
xmonadColors x = x { normalBorderColor  = "#111111"
                   , focusedBorderColor = windowBorderColor
                   , borderWidth        = 2
                   }

--------------------------------------------------------------------------------
decoTheme :: Theme
decoTheme = defaultTheme { decoHeight          = 14
                         , activeColor         = windowDecoColor
                         , activeBorderColor   = windowBorderColor
                         , activeTextColor     = "#222222"
                         , inactiveColor       = "#222222"
                         , inactiveBorderColor = "#222222"
                         , inactiveTextColor   = "#586e75"
                         , urgentColor         = "#dc5c5a"
                         , urgentBorderColor   = "#dc322f"
                         , urgentTextColor     = "#000000"
                         , fontName            = "xft:dejavu sans mono:size=9"
                         }
