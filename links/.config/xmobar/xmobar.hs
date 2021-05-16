import Xmobar
import System.Environment (getEnv)

config :: String -> Config
config igHome = defaultConfig {
  font = "xft:Ubuntu Nerd Font-12"
  , additionalFonts = ["Symbola-12"]
  , borderColor = "black"
  , border = NoBorder
  , bgColor = "#fffffa"
  , fgColor = "#00729b"
  , alpha = 255
  , position = TopW L 100
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = igHome ++ "/.config/xmobar/icons"
  , allDesktops = True
  , overrideRedirect = True
  , commands = [ Run $  Com (igHome ++ "/bin/trayer-padding-icon.sh") [] "trayerpad" 10
                , Run $ Date "%a %d/%m/%y %H:%M" "date" 10
                , Run $ UnsafeXMonadLog
                , Run $ BatteryP ["BAT0"] [
                    "-t", "<leftipat> <left>% / <timeleft>"
                    , "--"
                    , "--on-icon-pattern", "<icon=bat-ac/%%.xpm/>"
                    , "--off-icon-pattern", "<icon=bat/%%.xpm/>"
                ] 100
                , Run $ Kbd [("us", "US"), ("ru(phonetic)", "RU"), ("il", "IL")]
                , Run $ Wireless "wlo1" [
                    "-t", "<qualityipat> <ssid>"
                    , "--"
                    , "--quality-icon-pattern", "<icon=wireless/%%.xpm/>"
                    ] 100
                , Run $ Brightness [
                    "-t", "<ipat> <percent>"
                    , "--"
                    , "-D", "intel_backlight"
                    , "--brightness-icon-pattern", "<icon=brightness/%%.xpm/>"
                    ] 100
              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%UnsafeXMonadLog% }{ %wlo1wi% | %kbd% | %bright% | %battery% | %date%%trayerpad%"
}

main :: IO ()
main = do
    igHome <- getEnv "HOME"
    xmobar $ config igHome
