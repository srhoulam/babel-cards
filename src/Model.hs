{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Model where

import           Database.Persist.Class (ToBackendKey)
import           Database.Persist.Quasi (lowerCaseSettings)
import           Database.Persist.Sql   (Key, SqlBackend, fromSqlKey)
import           Database.Persist.TH
import           Database.Persist.Types (Entity)
import           Lens.Micro.TH
import           RIO
import           RIO.Text               (Text)
import           RIO.Time               (UTCTime)
import           Types.Review

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistmodels")

makeFields ''Entity
makeFields ''Card
makeFields ''Deck

data CardMetadata = CardMetadata
  { cardMetadataCardEntity :: !(Entity Card)
  , cardMetadataDecks      :: ![DeckId]
  , cardMetadataTags       :: ![TagId]
  }

data DeckMetadata = DeckMetadata
  { deckMetadataDeckEntity  :: !(Entity Deck)
  , deckMetadataCardCount   :: !Int
  , deckMetadataLastStudied :: !(Maybe UTCTime)
  }

keyToInt :: ToBackendKey SqlBackend record => Key record -> Int
keyToInt = fromIntegral . fromSqlKey
