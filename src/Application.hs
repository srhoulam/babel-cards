{-# LANGUAGE NoImplicitPrelude #-}
module Application (bootstrapBabel) where

import           Application.Database
import           Database.Persist.Sql   (runMigration)
-- import           Model
import           RIO
import           Types
import Application.TUI (lifecycle)

bootstrapBabel :: RIO Babel ()
bootstrapBabel = do
  runDB $ runMigration migrateAll
  lifecycle
