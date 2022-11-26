{-# LANGUAGE TemplateHaskell #-}
module Settings where

import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as Text (pack)
import           Database.Persist.Sqlite        (SqliteConf (..), fkEnabled,
                                                 mkSqliteConnectionInfo,
                                                 walEnabled)
import           Lens.Micro
import           System.Environment             (lookupEnv)
import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath.Posix

loadSettings :: IO SqliteConf
loadSettings = do
  dbOverride <- lookupEnv "BABEL_DATABASE"
  userDataDir <- getUserDataDir "babel-cards"
  let dbPath = fromMaybe
        (userDataDir </> "babel-cards.sqlite")
        dbOverride
      connString = mkSqliteConnectionInfo (Text.pack dbPath)
        & walEnabled .~ True
        & fkEnabled .~ True
  return $ SqliteConfInfo connString 1
