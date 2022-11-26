{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Application
import           Import
import           Import.Main
import           Options.Applicative.Simple
import qualified Paths_babel_cards
import           RIO.Process

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_babel_cards.version)
    "BabelCards command-line interface"
    "BabelCards is a flash-cards study aid."
    (CmdLineOptions
       <$> switch ( long "verbose"
                    <> short 'v'
                    <> help "Verbose output?")
    )
    empty

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  dbSettings <- loadSettings
  config <- loadConfig
  rngTV <- newTFGen >>= newTVarIO
  connPool <- createConnPool dbSettings
  withLogFunc lo $ \lf ->
    let babel = Babel
          { bLogFunc = lf
          , bProcessContext = pc
          , bOptions = options
          , bRNG = rngTV
          , bConfig = config
          , bDatabaseSettings = dbSettings
          , bConnPool = connPool
          }
     in runRIO babel bootstrapBabel
