module Main (main) where

import XMonad
import XMonad.Hooks.DynamicLog (statusBar, xmobarPP)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Data.List (intercalate)
import System.FilePath (dropFileName, (</>))

----------

keysOverrides :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keysOverrides conf@(XConfig {XMonad.modMask = modMask_}) =
  flip M.union (keys def conf) $ M.fromList 
    [ ((modMask_ .|. shiftMask, xK_Return), spawn =<< fromMaybe (XMonad.terminal conf) <$> io (lookupEnv "NIX_MONAD_XFCE4_TERMINAL"))

    , ((modMask_, xK_p), (spawn =<<) $ fmap withPathToSelf $ fromMaybe "dmenu_run" <$> io (lookupEnv "NIX_MONAD_DMENU_RUN"))
    , ((modMask_ .|. shiftMask, xK_p), spawn =<< fromMaybe "gmrun" <$> io (lookupEnv "NIX_MONAD_GMRUN"))
    
    , ((modMask_ .|. shiftMask, xK_slash), message help)
    , ((modMask_, xK_question), message help)

    , ((modMask_, xK_equal), keyboard "us")
    , ((modMask_, xK_0), keyboard "ru")

    , ((modMask_, xK_comma), volume "decrease")
    , ((modMask_, xK_period), volume "increase")
    , ((modMask_ .|. shiftMask, xK_comma), volume "mute")
    , ((modMask_ .|. shiftMask, xK_period), volume "unmute")
    ]
  where
    message xs = do
      m <- fromMaybe "xmessage" <$> io (lookupEnv "XMONAD_XMESSAGE")
      spawn $ intercalate " " [ m, "-fn \"monospace\"", "\"" ++ xs ++ "\"" ]

    keyboard xs = do
      k <- fromMaybe "setxkbmap" <$> io (lookupEnv "NIX_MONAD_SETXKBMAP")
      spawn $ intercalate " " [ k, xs ]

    volume xs = do
      v <- fromMaybe "volume_pulse" <$> io (lookupEnv "NIX_MONAD_VOLUME_PULSE")
      spawn $ intercalate " " [ v, xs ]

help :: String
help = unlines
  [ "Additional key-bindings:"
  , ""
  , "mod-=           Switch to US keyboard layout"
  , "mod-0           Switch to RU keyboard layout"
  , "mod-,           Decrease volume"
  , "mod-.           Increase volume"
  , "mod-Shift-,     Mute volume"
  , "mod-Shift-.     Unmute volume"
  ]

withPathToSelf :: FilePath -> String
withPathToSelf fp = 
  intercalate " "
    [ "export"
    , "PATH=" ++ intercalate ":" [ dropFileName fp, "$PATH" ]
    , "&&"
    , fp
    ]

----------

main :: IO ()
main = do

  xmobar_ <- fromMaybe "xmobar" <$> lookupEnv "NIX_MONAD_XMOBAR"

  let toggleStrutsKey XConfig {XMonad.modMask = modMask_} = (modMask_, xK_b)
      config_ = def { modMask = mod4Mask
                    , keys = keysOverrides
                    }

  xmonad =<< statusBar (withPathToSelf xmobar_) xmobarPP toggleStrutsKey config_

----------
