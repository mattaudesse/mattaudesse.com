module Site.Migrate (migrateAll) where

import Database.Persist.Sql (Migration)
import Site.Api.Contact     (migrateVisitorMsg)


migrateAll :: Migration
migrateAll =  do
  migrateVisitorMsg
