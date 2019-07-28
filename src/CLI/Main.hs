{-# LANGUAGE OverloadedStrings #-}
-- | 

module CLI.Main
  ( main
  ) where

import Options.Applicative
import CLI.Audio
import Control.Monad
import Data.List
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T
import System.Process

main :: IO ()
main = runAudio =<< execParser opts
  where
    opts = info (adjustVolumeParser <**> helper)
      ( fullDesc
     <> progDesc "Tools for managing a linux desktop"
     <> header "One Punch Desktop Tools" )

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
