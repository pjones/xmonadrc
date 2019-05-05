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
       , tabTheme
       , topBarTheme
       , base03, base02, base01, base00, base0, base1, base2, base3
       , yellow, orange, red, magenta, violet, blue, cyan, green
       , black, darkgray
       ) where

--------------------------------------------------------------------------------
import XMonad
import XMonad.Layout.Decoration

--------------------------------------------------------------------------------
xmonadColors :: XConfig a -> XConfig a
xmonadColors x = x { normalBorderColor  = "#444444"
                   , focusedBorderColor = "#ff52bb"
                   , borderWidth        = 5
                   }

--------------------------------------------------------------------------------
tabTheme :: Theme
tabTheme = def
  { fontName            = defaultFont
  , activeColor         = blue
  , inactiveColor       = darkgray
  , activeBorderColor   = blue
  , inactiveBorderColor = darkgray
  , activeTextColor     = base03
  , inactiveTextColor   = base00
  , decoHeight          = 14
  }

--------------------------------------------------------------------------------
-- | Stolen from: https://github.com/altercation/dotfiles-tilingwm
topBarTheme :: Theme
topBarTheme = def
  { fontName            = defaultFont
  , inactiveBorderColor = darkgray
  , inactiveColor       = darkgray
  , inactiveTextColor   = darkgray
  , activeBorderColor   = blue
  , activeColor         = blue
  , activeTextColor     = blue
  , urgentBorderColor   = magenta
  , urgentTextColor     = magenta
  , decoHeight          = 5
  }

--------------------------------------------------------------------------------
defaultFont :: String
defaultFont = "xft:Dejavu Sans Mono-9"

--------------------------------------------------------------------------------
-- | Stolen from: https://github.com/altercation/dotfiles-tilingwm
base03, base02, base01, base00, base0, base1, base2, base3 :: String
yellow, orange, red, magenta, violet, blue, cyan, green    :: String
black,  darkgray                                           :: String
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
black   = "#000000"
darkgray = "#222222"
