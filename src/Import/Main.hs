module Import.Main (module Import.Main) where

import           Control.Monad.Logger    (runNoLoggingT)
import           Database.Persist.Sql    (ConnectionPool)
import           Database.Persist.Sqlite (SqliteConf (..), createSqlitePool,
                                          createSqlitePoolFromInfo,
                                          sqlConnectionStr)

import           Import
import           Settings                as Import.Main (loadSettings)
import           System.Directory        (createDirectoryIfMissing)
import           System.Environment
import           System.FilePath.Posix
import           System.Random.TF.Init   as Import.Main (newTFGen)
import           Util.String             (mapText)

createConnPool :: BabelEmbeddedSettings -> IO ConnectionPool
createConnPool settings = do
  dataDir <- ensureDataDirExists settings
  runNoLoggingT $ case besDatabase settings of
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

-- | FIXME: actually load config!
loadConfig :: IO BabelConfig
loadConfig = return BabelConfig
  { bcMaxInterval = 864000 -- 10 days
  , bcMinInterval = 600 -- 10 minutes
  }
