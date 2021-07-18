module Util.String where

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text               as Text (pack, unpack)

decapitalize :: String -> String
decapitalize (a:as) = toLower a:as
decapitalize [] = []

mapText :: (String -> String) -> Text -> Text
mapText f = Text.pack . f . Text.unpack
