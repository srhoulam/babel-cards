{-# LANGUAGE NoImplicitPrelude #-}
module Application (babelMain) where

import           Database.Persist.Class (count)
import           Database.Persist.Sql   (Filter, SqlBackend, runMigration,
                                         runSqlPool)
import           Model
import           RIO
import           Types

type BabelQuery a = ReaderT SqlBackend (ReaderT Babel IO) a

babelExample :: RIO Babel Int
babelExample = runDB $ count ([] :: [Filter Item])

babelMain :: RIO Babel ()
babelMain = do
  runDB $ runMigration migrateAll
  _ <- babelExample
  return ()

runDB :: BabelQuery a -> RIO Babel a
runDB action = RIO $ do
  connPool <- asks bConnPool
  runSqlPool action connPool
