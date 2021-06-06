module CLI.CLI
  ( cliParser
  , Opts (..)
  ) where

import Data.Text (Text)
import Options.Applicative

import CLI.Theme
import CLI.Audio

data Opts = VolumeOpts AdjustVolume
          | ThemeOpts Text

cliParser :: Parser Opts
cliParser = (VolumeOpts <$> adjustVolumeParser) <|> (ThemeOpts <$> themeParser)
