module Import.Main (module Import.Main) where

import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist.Sql    (ConnectionPool)
import           Database.Persist.Sqlite (SqliteConf (..), createSqlitePool,
                                          createSqlitePoolFromInfo,
                                          sqlConnectionStr)

import           Import
import           Settings                as Import.Main (loadSettings)
import           System.Directory        (createDirectoryIfMissing)
import           System.Environment
import           System.FilePath.Posix
import           Util.String             (mapText)

createConnPool :: BabelEmbeddedSettings -> IO ConnectionPool
createConnPool settings = do
  dataDir <- ensureDataDirExists settings
  runStderrLoggingT $ case besDatabase settings of
    SqliteConf connStr poolSize ->
      createSqlitePool (mapText (dataDir </>) connStr) poolSize
    SqliteConfInfo confInfo poolSize -> createSqlitePoolFromInfo
      (over sqlConnectionStr (mapText (dataDir </>)) confInfo)
      poolSize

ensureDataDirExists :: BabelEmbeddedSettings -> IO FilePath
ensureDataDirExists settings = do
  home <- getEnv "HOME"
  let dataDir = home </> besUserDataDir settings
  createDirectoryIfMissing True dataDir
  return dataDir
