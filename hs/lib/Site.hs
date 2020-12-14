{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Site
  ( ConfigPath
  , SqlitePath
  , dumpContacts
  , serve
  , siteApp
  ) where

import Control.Exception               (IOException, catch)
import Control.Monad.Logger            (NoLoggingT(..))
import Data.Pool                       (Pool)
import Data.Text                       (toLower, pack, unpack)
import Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import Database.Persist.Sql            (SqlBackend, runMigration, runSqlPool)
import Database.Persist.Sqlite         (createSqlitePool)
import Database.Persist.Types          (Entity(..))
import Network.HTTP.Types              (status404)
import Network.HTTP.Types.Header       (hAccept, hContentType)
import Network.Wai                     (ResponseReceived)
import Network.Wai.Handler.Warp        (run)
import Servant                         ((:<|>)(..), (:>))
import Servant.Server.StaticFiles      (serveDirectoryWith)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types              (StaticSettings(..), unsafeToPiece)

import System.IO
  ( BufferMode(LineBuffering, NoBuffering)
  , hSetBuffering
  , stderr
  , stdin
  , stdout
  )

import qualified Data.ByteString as BS
import qualified Data.Yaml       as Y
import qualified Network.Wai     as N
import qualified Servant         as S
import qualified Site.Core       as C

import Site.Api         (Api, apiServerFrom)
import Site.Api.Contact (formattedWithRecordId, visitorMsgsByCreationAsc)
import Site.Core        (Env(..), Config(..), info)
import Site.Migrate     (migrateAll)


--------------------------------------------------------------------------------

type ConfigPath = FilePath
type SqlitePath = FilePath


--------------------------------------------------------------------------------

dumpContacts :: SqlitePath -> IO ()
dumpContacts db = do
  cs <- createPool db >>= runSqlPool visitorMsgsByCreationAsc

  let msg c = pack ((take 50 $ repeat '-') ++ "\n")
           <> formattedWithRecordId (entityVal c) (entityKey c)

  mapM_ (putStrLn . unpack . msg) cs


--------------------------------------------------------------------------------

handle404ApplicationJson :: N.Response
handle404ApplicationJson =
  N.responseLBS status404 [C.contentTypeApplicationJson] ""

handle404TextHtml :: N.Response
handle404TextHtml =
  N.responseFile status404 [C.contentTypeTextHtml] "dist/404/index.html" Nothing

handle404 :: N.Application
handle404 req res =
  handle `catch` fallback

  where fallback :: IOException -> IO ResponseReceived
        fallback _ = res $ N.responseLBS status404 [C.contentTypeTextHtml] ""

        handle = res $ if isAppjson
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
siteApp env = S.serve (S.Proxy :: S.Proxy Site)
     $ apiServerFrom      env
  :<|> serveDirectoryWith staticWebAppSettings


createPool :: SqlitePath -> IO (Pool SqlBackend)
createPool db = runNoLoggingT $ createSqlitePool (pack db) 8


serve :: SqlitePath -> ConfigPath -> IO ()
serve db conf = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  conf' <- BS.readFile conf >>= Y.decodeThrow
  pool  <- createPool db
  runSqlPool (runMigration migrateAll) pool

  let env = Env pool conf'

  info . pack $ "Launching site on port: " ++ show (port conf')

  run (port conf') (siteApp env)
