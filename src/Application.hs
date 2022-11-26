{-# LANGUAGE NoImplicitPrelude #-}
module Application (bootstrapBabel) where

import           Application.Database
import           Database.Persist.Sql (runMigration)
-- import           Model
import           Application.TUI      (lifecycle)
import           RIO
import           Types

bootstrapBabel :: RIO Babel ()
bootstrapBabel = do
  runDB $ runMigration migrateAll
  lifecycle
