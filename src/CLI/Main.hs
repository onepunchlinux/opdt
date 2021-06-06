{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | 

module CLI.Main
  ( main
  ) where

import Options.Applicative
import Control.Monad
import Data.List
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import System.Process
import Dhall (Generic, FromDhall)
import qualified Dhall as D
import Data.Aeson
import qualified Data.Aeson.Yaml as Yaml
import System.Directory

import CLI.CLI (cliParser, Opts(..))
import CLI.Audio
import Data.Theme
import CLI.Emacs (setThemeFile, reloadTheme)

main :: IO ()
main = runCLI =<< execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Tools for managing a linux desktop"
     <> header "One Punch Desktop Tools" )

runCLI :: Opts -> IO ()
runCLI (VolumeOpts vOpts) = runAudio vOpts
runCLI (ThemeOpts tOpts) = runTheme tOpts

runTheme :: Text -> IO ()
runTheme themeName = do
  cfg <- D.input D.auto "~/.config/opdt/config.dhall"
  case M.lookup themeName (themes cfg) of
    Nothing -> error $ T.unpack ("theme " <> themeName <> " not found")
    (Just theme) -> do
      setAlacrittyTheme theme
      setEmacsTheme theme

setAlacrittyTheme :: Theme -> IO ()
setAlacrittyTheme theme = do
  let alacrittyTheme = Yaml.encode (AlacrittyConfig (AlacrittyColors
                                                     { primary = primaryColors theme
                                                     , normal = normalColors theme
                                                     , bright = brightColors theme
                                                     }))
  homeDir <- getHomeDirectory
  BS.writeFile (homeDir ++ "/.config/opdt/alacritty-theme.yml") alacrittyTheme 

setEmacsTheme :: Theme -> IO ()
setEmacsTheme theme = do
  setThemeFile theme
  reloadTheme

runAudio :: AdjustVolume -> IO ()
runAudio adjustOption = do
  sink <- findSink
  case sink of
    Just s -> adjust s adjustOption
    Nothing -> pure ()

adjust :: String -> AdjustVolume -> IO ()
adjust sink (Increment t) = do
  pactlMute sink "0"
  pactlSetVolume sink $ "+" <> T.unpack t
adjust sink (Decrement t) = do
  pactlMute sink "0"
  pactlSetVolume sink $ "-" <> T.unpack t
adjust sink Mute = do
  pactlMute sink "toggle"

findSink :: IO (Maybe String)
findSink = do
  (_, stdout, _) <- readProcessWithExitCode "pactl" ["list", "short", "sinks"] ""
  let sinks = lines stdout
      maybeBluezSink = head . words <$> find (isInfixOf "bluez") sinks
      maybeRunningSink = head . words <$> find (\t -> not (isInfixOf "bluez" t) && isInfixOf "RUNNING" t) sinks
  if isJust maybeBluezSink && isNothing maybeRunningSink
    then pure maybeBluezSink
    else pure maybeRunningSink


pactlMute :: String -> String -> IO ()
pactlMute sink opt = void $
  readProcessWithExitCode "pactl" ["set-sink-mute", sink, opt] ""

pactlSetVolume :: String -> String -> IO ()
pactlSetVolume sink opt = void $
  readProcessWithExitCode "pactl" ["set-sink-volume", sink, opt] ""
