{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Site
    ( dumpContacts
    , serve
    , siteApp
    ) where

import Control.Monad.Logger            (NoLoggingT(..))
import Data.Monoid                     ((<>))
import Data.Pool                       (Pool)
import Data.Text                       (toLower, pack, unpack)
import Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import Database.Persist.Sql            (SqlBackend, runMigration, runSqlPool)
import Database.Persist.Sqlite         (createSqlitePool)
import Database.Persist.Types          (Entity(..))
import Network.HTTP.Types              (status404)
import Network.HTTP.Types.Header       (hAccept, hContentType)
import Network.Wai.Handler.Warp        (run)
import Servant                         ((:<|>)(..), (:>))
import Servant.Utils.StaticFiles       (serveDirectoryWith)
import System.IO                       (FilePath)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types              (StaticSettings(..), unsafeToPiece)

import qualified Data.ByteString as BS
import qualified Data.Yaml       as Y
import qualified Network.Wai     as N
import qualified Servant         as S

import Site.Api         (Api, apiServerFrom)
import Site.Api.Contact (formattedWithRecordId, visitorMsgsByCreationAsc)
import Site.Core        (Env(..), Config(..), info)
import Site.Migrate     (migrateAll)


--------------------------------------------------------------------------------

dumpContacts :: IO ()
dumpContacts = do
    pool <- createPool
    cs   <- runSqlPool visitorMsgsByCreationAsc pool

    let msg c = pack ((take 50 $ repeat '-') ++ "\n")
             <> formattedWithRecordId (entityVal c) (entityKey c)

    mapM_ (putStrLn . unpack . msg) cs


--------------------------------------------------------------------------------

handle404ApplicationJson :: N.Response
handle404ApplicationJson =
    N.responseLBS status404 [("Content-Type", "application/json")] ""

handle404TextHtml :: N.Response
handle404TextHtml =
    N.responseFile status404 [("Content-Type", "text/html")] "dist/404.html" Nothing

handle404 :: N.Application
handle404 req res = res $ if isAppjson
    then handle404ApplicationJson
    else handle404TextHtml

    where appJson   = encodeUtf8 "application/json"
          toLower'  = encodeUtf8 . toLower . decodeUtf8
          headers   = (fmap . fmap) toLower' $ N.requestHeaders req
          isAppjson = (hAccept,      appJson) `elem` headers
                   || (hContentType, appJson) `elem` headers


--------------------------------------------------------------------------------

staticWebAppSettings :: StaticSettings
staticWebAppSettings =
    (defaultWebAppSettings ("./dist" :: FilePath))
    { ssIndices    = [ unsafeToPiece "index.html" ]
    , ss404Handler = Just handle404
    }


type Site = ("api" :> Api) :<|> S.Raw


siteApp :: Env -> N.Application
siteApp env =
    S.serve proxy (apiServerFrom env :<|> distRaw)

    where distRaw = serveDirectoryWith staticWebAppSettings
          proxy   :: S.Proxy Site
          proxy   =  S.Proxy


createPool :: IO (Pool SqlBackend)
createPool = runNoLoggingT $ createSqlitePool "mattaudesse.com.db" 8


serve :: IO ()
serve = do
    confRaw <- BS.readFile "mattaudesse.com.yaml"
    conf    <- Y.decodeThrow confRaw
    pool    <- createPool
    runSqlPool (runMigration migrateAll) pool

    let env = Env pool conf

    info . pack $ "Launching site on port: " ++ show (port conf)

    run (port conf) (siteApp env)
