{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Review where

import           Data.Either            (Either (..))
import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql   (PersistFieldSql (..))
import           Database.Persist.Types (PersistValue (..), SqlType (..))
import           Prelude                (Enum (..), fromIntegral, ($), (.))

data ReviewEase =
  ReviewAgain
  | ReviewHard
  | ReviewGood
  | ReviewEasy
  deriving (Enum)

instance PersistField ReviewEase where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 num) = Right $ toEnum $ fromIntegral num
  fromPersistValue _ = Left "Persist: non-integer representation of ReviewEase"

instance PersistFieldSql ReviewEase where
  sqlType _ = SqlInt32
