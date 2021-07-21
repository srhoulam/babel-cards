{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Review where

import           Data.Either            (Either (..))
import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql   (PersistFieldSql (..))
import           Database.Persist.Types (PersistValue (..), SqlType (..))
import           Prelude                (Bounded, Enum (..), Show, fromIntegral,
                                         ($), (.))

data QueueIndex =
  NewQueue
  | LearningQueue
  | ReviewQueue
  deriving (Bounded, Enum, Show)

data ReviewEase =
  ReviewAgain
  | ReviewHard
  | ReviewGood
  | ReviewEasy
  deriving (Bounded, Enum, Show)

instance PersistField QueueIndex where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 num) = Right $ toEnum $ fromIntegral num
  fromPersistValue _ = Left "Persist: non-integer representation of QueueIndex"
instance PersistFieldSql QueueIndex where
  sqlType _ = SqlInt32

instance PersistField ReviewEase where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 num) = Right $ toEnum $ fromIntegral num
  fromPersistValue _ = Left "Persist: non-integer representation of ReviewEase"
instance PersistFieldSql ReviewEase where
  sqlType _ = SqlInt32
