{-# LANGUAGE DeriveGeneric     #-}

module Data.Theme
  ( ColorGroup (..)
  , PrimaryColorGroup (..)
  , EditorColorGroup (..)
  , Theme (..)
  , Config (..)
  , AlacrittyColors (..)
  , AlacrittyConfig (..)
  ) where

import Data.Text
import qualified Data.Map as M
import Dhall
import Data.Aeson

data ColorGroup = ColorGroup
  { black :: Text
  , red :: Text
  , green :: Text
  , yellow :: Text
  , blue :: Text
  , magenta :: Text
  , cyan :: Text
  , white :: Text
  } deriving (Generic, Show)

data PrimaryColorGroup = PrimaryColorGroup
  { background :: Text
  , foreground :: Text
  } deriving (Generic, Show)

data EditorColorGroup = EditorColorGroup
  { dark0Soft :: Text
  , dark1 :: Text
  , dark2 :: Text
  , dark3 :: Text
  , dark4 :: Text
  , gray :: Text
  , light0Hard :: Text
  , light0 :: Text
  , light1 :: Text
  , light2 :: Text
  , light3 :: Text
  , light4 :: Text
  , brightOrange :: Text
  , normalOrange :: Text
  , darkRed :: Text
  , darkBlue :: Text
  , darkAqua :: Text
  , sienna :: Text
  , lightblue4 :: Text
  , burlywood4 :: Text
  , turquoise4 :: Text
  , currentDiffA :: Text
  , currentDiffB :: Text
  , currentDiffC :: Text
  , currentDiffAncestor :: Text
  , fineDiffA :: Text
  , fineDiffB :: Text
  , fineDiffC :: Text
  , fineDiffAncestor :: Text
  } deriving (Generic, Show)


data Theme = Theme
  { primaryColors :: PrimaryColorGroup
  , normalColors :: ColorGroup
  , brightColors :: ColorGroup
  , editorColors :: EditorColorGroup
  } deriving (Generic, Show)

data Config = Config
  { themes :: M.Map Text Theme
  } deriving (Generic, Show)

instance FromDhall ColorGroup
instance ToJSON ColorGroup

instance FromDhall PrimaryColorGroup
instance ToJSON PrimaryColorGroup

instance FromDhall Config

instance FromDhall EditorColorGroup

instance FromDhall Theme

data AlacrittyColors = AlacrittyColors
  { primary :: PrimaryColorGroup
  , normal :: ColorGroup
  , bright :: ColorGroup
  } deriving Generic

instance ToJSON AlacrittyColors
newtype AlacrittyConfig = AlacrittyConfig {colors :: AlacrittyColors} deriving Generic

instance ToJSON AlacrittyConfig
