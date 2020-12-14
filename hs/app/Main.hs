module Main (main) where

import Control.Monad (join)

import Options.Applicative
  ( CommandFields
  , Mod
  , Parser
  , (<**>)
  , action
  , command
  , execParser
  , footer
  , fullDesc
  , helper
  , hsubparser
  , info
  , long
  , metavar
  , option
  , str
  )

import Site (ConfigPath, SqlitePath, dumpContacts, serve)


sqlitePath :: Parser SqlitePath
sqlitePath = option str
  $ long    "db"
 <> metavar "SQLite database path"
 <> action  "file"


configPath :: Parser ConfigPath
configPath = option str
  $ long    "conf"
 <> metavar "config YAML path"
 <> action  "file"


dumpContacts' :: Mod CommandFields (IO ())
dumpContacts' = command "dump-contacts"
  $ info (dumpContacts <$> sqlitePath) mempty


serve' :: Mod CommandFields (IO ())
serve' = command "serve"
  $ info (serve <$> sqlitePath <*> configPath) mempty


main :: IO ()
main = join . execParser
  $ info ((hsubparser $ dumpContacts' <> serve') <**> helper)
  $ fullDesc
 <> footer "https://mattaudesse.com"
