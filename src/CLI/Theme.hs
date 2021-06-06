module CLI.Theme
  ( themeParser
  ) where

import Data.Text (Text)
import Options.Applicative

themeParser :: Parser Text
themeParser = strOption
  ( long "theme"
 <> short 't'
 <> metavar "THEME"
 <> help "select theme"
  )
