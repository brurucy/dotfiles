import XMonad

--- For Xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedScratchpad
--- For Layouts
import XMonad.Layout.Spiral
import Data.Ratio
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
--- For Keys
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO
--- Startup hooks
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce
--- Gridselect
import XMonad.Actions.GridSelect

myLayouts = spacing 10 $
            layoutTall ||| layoutSpiral ||| layoutGrid ||| layoutMirror ||| layoutFull
    where
      layoutTall = Tall 1 (3/100) (1/2)
      layoutSpiral = spiral (125 % 146)
      layoutGrid = Grid
      layoutMirror = Mirror (Tall 1 (3/100) (3/5))
      layoutFull = Full

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-scale ~/.background-image/current_wallpaper.png"
  spawnOnce "emacs --daemon"
  setWMName "LG3D"

main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/rucy/.config/xmobar/xmobarrc"
    xmonad $ docks $ def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts  $ myLayouts
        , terminal = "kitty"
        , normalBorderColor  = "#ea51b2"
        , focusedBorderColor = "#b45bcf"
        , borderWidth = 5
        , startupHook = myStartupHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppSort = fmap (namedScratchpadFilterOutWorkspace .) (ppSort def)
                        , ppCurrent = xmobarColor "#c792ea" "#c792ea" . wrap "" " "
                        , ppVisible = xmobarColor "#ab47bc" "#ab47bc" . wrap "" " "
                        , ppHidden = xmobarColor "#ab47bc" "#ab47bc" . wrap "" " "
                        , ppHiddenNoWindows = xmobarColor "#4d4f68" "#4d4f68" . wrap "" " "
                        , ppLayout = xmobarColor "#282936" "#ea51b2:0" . wrap " " " "
                        , ppSep = " | "
                        , ppTitle = const ""
                        }
        , modMask = mod4Mask
        } `additionalKeys`
        [ ((mod4Mask, xK_p), spawn "rofi -show run")
        , ((0,  xK_Print), spawn "flameshot gui")
        , ((0 , xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
        , ((0 , xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
        , ((0 , xF86XK_AudioMute), spawn "amixer -q set Master toggle")
        , ((0 , xF86XK_MonBrightnessUp), spawn "brightnessctl s 100+")    
        , ((0 , xF86XK_MonBrightnessDown), spawn "brightnessctl s 100-")
        , ((mod4Mask, xF86XK_Display), spawn "betterlockscreen -l dimblur")
        , ((mod4Mask, xK_z), spawn "emacsclient --create-frame --alternate-editor=\"\"")
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        ]
