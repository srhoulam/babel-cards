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

createConnPool :: FilePath -> BabelEmbeddedSettings -> IO ConnectionPool
createConnPool dataDir' settings = do
  dataDir <- ensureDataDirExists dataDir'
  runNoLoggingT $ case besDatabase settings of
    SqliteConf connStr poolSize ->
      createSqlitePool (mapText (dataDir </>) connStr) poolSize
    SqliteConfInfo confInfo poolSize -> createSqlitePoolFromInfo
      (over sqlConnectionStr (mapText (dataDir </>)) confInfo)
      poolSize

ensureDataDirExists :: FilePath -> IO FilePath
ensureDataDirExists dataDir =
  createDirectoryIfMissing True dataDir
  >> return dataDir

loadConfig :: IO BabelConfig
loadConfig = do
  maxIntervalStr <- lookupEnv "BABEL_MAX_INTERVAL"
  minIntervalStr <- lookupEnv "BABEL_MIN_INTERVAL"

  let maxInterval = maybe 864000 {- 10 days -} fromIntegral
                    $ maxIntervalStr >>= readMaybe
      minInterval = maybe 600 {- 10 minutes -} fromIntegral
                    $ minIntervalStr >>= readMaybe

  return BabelConfig
    { bcMaxInterval = maxInterval
    , bcMinInterval = minInterval
    }
