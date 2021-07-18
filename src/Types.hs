{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           Data.Aeson              (FromJSON (..), Options (..),
                                          defaultOptions, genericParseJSON)
import           Database.Persist.Sql    (ConnectionPool)
import           Database.Persist.Sqlite (SqliteConf)
import           GHC.Generics
import           RIO
import           RIO.Process
import           Util.String             (decapitalize)

-- | Command line arguments
data CmdLineOptions = CmdLineOptions
  { optionsVerbose :: !Bool
  }

data Babel = Babel
  { bLogFunc          :: !LogFunc
  , bProcessContext   :: !ProcessContext
  , bOptions          :: !CmdLineOptions
  , bConnPool         :: !ConnectionPool
  , bEmbeddedSettings :: !BabelEmbeddedSettings
  }

data BabelEmbeddedSettings = BabelEmbeddedSettings
  { besUserDataDir :: !String
  -- ^ User data directory, as a relpath from the
  -- user's $HOME.
  , besDatabase :: !SqliteConf
  } deriving Generic

instance FromJSON BabelEmbeddedSettings where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = decapitalize . drop 3 }

instance HasLogFunc Babel where
  logFuncL = lens bLogFunc (\x y -> x { bLogFunc = y })
instance HasProcessContext Babel where
  processContextL = lens bProcessContext (\x y -> x { bProcessContext = y })
