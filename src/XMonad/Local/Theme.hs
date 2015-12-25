--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Color theme.
module XMonad.Local.Theme
       ( xmonadColors
       , tabColors
       ) where

--------------------------------------------------------------------------------
import XMonad
import XMonad.Layout.Decoration

--------------------------------------------------------------------------------
xmonadColors :: XConfig a -> XConfig a
xmonadColors x = x { normalBorderColor  = "#111111"
                   , focusedBorderColor = "#6c71c4"
                   , borderWidth        = 4
                   }

--------------------------------------------------------------------------------
tabColors :: Theme
tabColors = Theme { activeColor         = "#6c71c4"
                  , inactiveColor       = "#111111"
                  , urgentColor         = "#ff0000"
                  , activeBorderColor   = "#6c71c4"
                  , inactiveBorderColor = "#111111"
                  , urgentBorderColor   = "#ff0000"
                  , activeTextColor     = "#111111"
                  , inactiveTextColor   = "#888888"
                  , urgentTextColor     = "#111111"
                  , fontName            = "xft:Dejavu Sans Mono-9"
                  , decoWidth           = 200
                  , decoHeight          = 18
                  , windowTitleAddons   = []
                  , windowTitleIcons    = []
                  }
