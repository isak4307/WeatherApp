{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_WeatherApp (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "WeatherApp"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Weather application ran in the terminal and through a bot in discord"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
