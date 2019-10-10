module Main where

import Data.List            (intercalate)
import System.Environment   (getArgs)
import System.Exit          (ExitCode(ExitFailure))
import System.Posix.Process (exitImmediately)
import Site                 (dumpContacts, serve)


printHelpAndExit :: IO ()
printHelpAndExit = do
  let msg = [ "Invalid argument; must be one of:"
            , "* serve"
            , "* dump-contacts"
            ]

  putStrLn $ intercalate "\n" msg
  exitImmediately $ ExitFailure 1


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["serve"]         -> serve
    ["dump-contacts"] -> dumpContacts
    _                 -> printHelpAndExit
