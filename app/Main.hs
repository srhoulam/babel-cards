{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Application
import Import
import Import.Main
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_babel_cards

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_babel_cards.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (CmdLineOptions
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  settings <- loadSettings
  connPool <- createConnPool settings
  withLogFunc lo $ \lf ->
    let babel = Babel
          { bLogFunc = lf
          , bProcessContext = pc
          , bOptions = options
          , bEmbeddedSettings = settings
          , bConnPool = connPool
          }
     in runRIO babel babelMain
