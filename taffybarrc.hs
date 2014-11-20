--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import System.Taffybar
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Weather
-- import System.Taffybar.Widgets.PollingBar
-- import System.Taffybar.Widgets.PollingGraph

--------------------------------------------------------------------------------
pagerConfig :: PagerConfig
pagerConfig = defaultPagerConfig
  { activeWindow     = colorize "white" "" . escape . shorten 10
  , activeWorkspace  = colorize "#88b324" "" . wrap "[" "]" . escape
  , visibleWorkspace = wrap "(" ")" . escape
  , hiddenWorkspace  = escape
  , emptyWorkspace   = const ""
  , urgentWorkspace  = colorize "#1c1c1c" "#d33682" . wrap "<" ">" . escape
  , widgetSep        = colorize "#5c5c5c" "" " | "
  }

--------------------------------------------------------------------------------
weatherConfig :: WeatherConfig
weatherConfig = (defaultWeatherConfig "KBJC") {weatherTemplate = withColor}
  where
    template  = "°F:$tempF$ °C:$tempC$ H:$humidity$"
    withColor = wrap "<span fgcolor='#859900'>" "</span>" template

--------------------------------------------------------------------------------
main :: IO ()
main = taffybarMain defaultTaffybarConfig
    { startWidgets  = [pager, note]
    , endWidgets    = [tray, clock, wea, mpris]
    , monitorNumber = 1 -- FIXME: this is for hawkins!
    , barHeight     = 22
    , barPosition   = Bottom
    }

  where
    clock  = textClockNew Nothing clkfmt 1
    clkfmt = "<span fgcolor='#268bd2'>%a %b %d, %H:%M</span>"
    pager  = taffyPagerNew pagerConfig
    note   = notifyAreaNew defaultNotificationConfig
    wea    = weatherNew weatherConfig 15
    mpris  = mprisNew
    tray   = systrayNew
