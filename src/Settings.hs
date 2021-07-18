{-# LANGUAGE TemplateHaskell #-}
module Settings where

import Data.ByteString
import Data.FileEmbed
import Data.Yaml
import Types

loadSettings :: IO BabelEmbeddedSettings
loadSettings = decodeThrow embeddedSettings

embeddedSettings :: ByteString
embeddedSettings = $(embedFile "config/settings.yaml")
