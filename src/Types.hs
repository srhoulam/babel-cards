{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where

import           Data.Aeson              (Options (..), defaultOptions)
import           Data.Aeson.TH
import           Database.Persist.Sql    (ConnectionPool, SqlBackend)
import           Database.Persist.Sqlite (SqliteConf)
import           GHC.Generics
import           RIO
import           RIO.Process
import           RIO.Time                (NominalDiffTime)
import           System.Random.TF
import           Util.String             (decapitalize)

type BabelQuery a = ReaderT SqlBackend (ReaderT Babel IO) a

-- | Command line arguments
data CmdLineOptions = CmdLineOptions
  { optionsVerbose :: !Bool
  }

data Babel = Babel
  { bLogFunc          :: !LogFunc
  , bProcessContext   :: !ProcessContext
  , bOptions          :: !CmdLineOptions
  , bConnPool         :: !ConnectionPool
  , bRNG              :: !(TVar TFGen)
  , bConfig           :: !BabelConfig
  , bDatabaseSettings :: !SqliteConf
  }

data BabelConfig = BabelConfig
  { bcMaxInterval :: !NominalDiffTime
  , bcMinInterval :: !NominalDiffTime
  }

$(deriveJSON defaultOptions { fieldLabelModifier = decapitalize . drop 2} 'BabelConfig)

instance HasLogFunc Babel where
  logFuncL = lens bLogFunc (\x y -> x { bLogFunc = y })
instance HasProcessContext Babel where
  processContextL = lens bProcessContext (\x y -> x { bProcessContext = y })
