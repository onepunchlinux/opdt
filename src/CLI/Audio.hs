-- | 

module CLI.Audio
  ( AdjustVolume (..)
  , adjustVolumeParser
  ) where

import Data.Text (Text)
import Options.Applicative

data AdjustVolume
  = Increment Text
  | Decrement Text
  | Mute

incrementVolume :: Parser AdjustVolume
incrementVolume = Increment <$> strOption
  ( long "inc"
 <> short 'i'
 <> metavar "INCREMENT"
 <> help "EXAMPLE: 5%"
  )

decrementVolume :: Parser AdjustVolume
decrementVolume = Decrement <$> strOption
  ( long "dec"
 <> short 'd'
 <> metavar "DECREMENT"
 <> help "EXAMPLE: 5%"
  )

muteVolume :: Parser AdjustVolume
muteVolume = flag Mute Mute 
  ( long "mute"
 <> short 'm'
 <> help "mute curently active sink"
  )

adjustVolumeParser :: Parser AdjustVolume
adjustVolumeParser = incrementVolume <|> decrementVolume <|> muteVolume
