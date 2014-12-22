{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Taffybar.Local.Widgets
       ( clock
       , pager
       , battery
       , weather
       , mpris
       , tray
       ) where

--------------------------------------------------------------------------------
import Graphics.UI.Gtk (Widget)
import System.Taffybar.Battery
import System.Taffybar.MPRIS
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Weather
import System.Taffybar.Widgets.PollingBar (BarConfig (..), defaultBarConfig)

-- To remind me for future tweaking.
-- import System.Taffybar.Widgets.PollingBar
-- import System.Taffybar.Widgets.PollingGraph

--------------------------------------------------------------------------------
import Taffybar.Local.MPRIS

--------------------------------------------------------------------------------
clock :: IO Widget
clock = textClockNew Nothing format 1
  where
    format :: String
    format = "<span fgcolor='#268bd2'>%a %b %d, %H:%M</span>"

--------------------------------------------------------------------------------
pager :: PagerConfig -> IO Widget
pager = taffyPagerNew

--------------------------------------------------------------------------------
battery :: IO Widget
battery = batteryBarNew barCfg 60
  where
    barCfg :: BarConfig
    barCfg = (defaultBarConfig colorFun) { barBorderColor = borderColor
                                         , barWidth       = 10
                                         }

    borderColor :: (Double, Double, Double)
    borderColor = (0.2, 0.2, 0.2)

    colorFun :: Double -> (Double, Double, Double)
    colorFun pct | pct > 0.99 = (0.521568627451, 0.6, 0.0)
                 | pct < 0.2  = (0.862745098039, 0.360784313725, 0.352941176471)
                 | otherwise  = borderColor

--------------------------------------------------------------------------------
weather :: IO Widget
weather = weatherNew weatherConfig 15
  where
    weatherConfig :: WeatherConfig
    weatherConfig = (defaultWeatherConfig "KBJC") {weatherTemplate = withColor}

    withColor :: String
    withColor = wrap "<span fgcolor='#859900'>" "</span>" template

    template :: String
    template  = "°F:$tempF$ °C:$tempC$ H:$humidity$%"

--------------------------------------------------------------------------------
mpris :: IO Widget
mpris  = mprisNew mprisConfig

--------------------------------------------------------------------------------
tray :: IO Widget
tray = systrayNew
