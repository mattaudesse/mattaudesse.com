{-# LANGUAGE OverloadedStrings #-}
module Spec (main) where

import Control.Monad.Logger    (NoLoggingT(..))
import Database.Persist.Class  (deleteWhere)
import Database.Persist.Sql    (runMigrationSilent, runSqlPool, runSqlPersistMPool)
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist.Types  (Filter)
import Test.Hspec              (before_, describe, hspec)

import Site.Api.Contact     (VisitorMsg, migrateVisitorMsg)
import Site.Api.ContactSpec (contactHandler, visitorMsgValidation)
import Site.Api.PingSpec    (pingHandler)
import Site.Core            (Config(..), Env(..))


main :: IO ()
main = do
    -- NB: in-memory pools of > 1 connection break since the scheduler will
    -- automatically close them - see:
    -- https://www.stackage.org/haddock/lts-11.20/persistent-sqlite-2.8.1.2/Database-Persist-Sqlite.html#v:createSqlitePool
    pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
    _    <- runSqlPool (runMigrationSilent migrateVisitorMsg) pool

    let conf = Config 0 "" "" "" "smtp-sender-email-addr"
        env  = Env    pool conf

        flushDb = flip runSqlPersistMPool pool $ do
            deleteWhere ([] :: [Filter VisitorMsg])


    hspec $ before_ flushDb $ do
        visitorMsgValidation

        describe "HTTP" $ do
            pingHandler    env
            contactHandler env
