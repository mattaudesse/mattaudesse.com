{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Site.Static (build) where

import Control.Monad.IO.Class     (liftIO)
import Data.Aeson                 (ToJSON, toJSON)
import Data.Foldable              (traverse_)
import Data.Monoid                ((<>))
import Data.String                (IsString)
import Development.Shake          ((%>), (~>))
import Development.Shake.FilePath ((</>), dropDirectory1)
import GHC.Generics               (Generic)
import Slick                      (compileTemplate', substitute)

import qualified Clay              as C
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as TL
import qualified Development.Shake as S
import qualified Text.Pandoc       as P

import Site.Core       (MonadUTC(..))
import Site.Static.Css (css)


--------------------------------------------------------------------------------

type Title       = String
type Copyright   = String
type Description = String

data CtxRoot = CtxRoot
    { title          :: Title
    , copyright      :: Copyright
    , description    :: Description
    , gitVerifyBlock :: String
    } deriving (Generic, Show)

data CtxContact = CtxContact
    { title       :: Title
    , copyright   :: Copyright
    , description :: Description
    } deriving (Generic, Show)

data Ctx404 = Ctx404
    { title       :: Title
    , copyright   :: Copyright
    , description :: Description
    } deriving (Generic, Show)

instance ToJSON CtxRoot
instance ToJSON CtxContact
instance ToJSON Ctx404

ctxRoot :: Copyright -> String -> CtxRoot
ctxRoot cp gitVerifyBlock = CtxRoot
    { title       = "Matt Audesse"
    , copyright   = cp
    , description = "I'm a software engineer who works for startups and\
                   \ distributed tech businesses remotely from Maine.\
                   \ This is my personal website, built with Haskell and\
                   \ PureScript."
    , gitVerifyBlock
    }

ctxContact :: Copyright -> CtxContact
ctxContact cp = CtxContact
    { title       = "Say hello"
    , copyright   = cp
    , description = "Feel free to contact me for business reasons or\
                   \ just to say hi."
    }

ctx404 :: Copyright -> Ctx404
ctx404 cp = Ctx404
    { title       = "Not found"
    , copyright   = cp
    , description = "404 Not Found"
    }

--------------------------------------------------------------------------------

mkCopyright :: MonadUTC m => m String
mkCopyright =  do
    cy <- utcCurrentYear
    let years = case cy `compare` "2018" of
            GT -> "2018 - " <> cy
            _  -> "2018"
    pure $ "© " <> years <> " Matt Audesse"


--------------------------------------------------------------------------------

staticToDist :: FilePath -> FilePath
staticToDist p = "dist" </> dropDirectory1 p

outNameFor :: (IsString m, Monoid m) => m -> m
outNameFor n = "dist" <> n <> "/index.html"

mdToHtml :: FilePath -> S.Action String
mdToHtml path = liftIO $ do
    let exts = P.def { P.readerExtensions = P.githubMarkdownExtensions }

    res <- P.runIO $ do
        doc <- liftIO $ readFile path
        ast <- P.readMarkdown exts $ T.pack doc
        P.writeHtml5String P.def ast

    html <- P.handleError res
    pure $ T.unpack html


--------------------------------------------------------------------------------

validateHtml :: FilePath -> S.Action ()
validateHtml f =
    S.cmd_ (S.EchoStdout False) ["htmlhint", f]

(%>:) :: ToJSON ctx => String -> S.Action ctx -> S.Rules ()
n %>: f =
    let tmplName = "static/templates" <> n <> "/index.tmpl"
        outName  = outNameFor n

    in outName %> \out -> do
        ctx  <- f
        tmpl <- compileTemplate' tmplName
        let html = T.unpack $ substitute tmpl (toJSON ctx)
        S.writeFile' out html
        validateHtml out


--------------------------------------------------------------------------------

build :: IO ()
build =  do
    let opts = S.shakeOptions { S.shakeThreads = 0, S.shakeColor = True }
    copyright' <- mkCopyright

    S.shakeArgs opts $ do
        let copyStatic p       = S.copyFileChanged p (staticToDist p)
            cssPath            = "dist/assets/style/mattaudesse.com.css"
            gpgPath            = "static/matt-audesse-git.asc"
            gitVerifyBlockPath = "static/templates/_git-verify-block.md"

        "site" ~> S.need [ outNameFor  "/"
                         , outNameFor  "/contact"
                         , outNameFor  "/404"
                         , staticToDist gpgPath
                         , cssPath
                         , "static-assets"
                         ]

        "/" %>: do
            S.need [gitVerifyBlockPath]
            gvb <- mdToHtml gitVerifyBlockPath
            pure $ ctxRoot copyright' gvb

        "/contact" %>: pure (ctxContact copyright')
        "/404"     %>: pure (ctx404     copyright')

        staticToDist gpgPath ~>
            copyStatic gpgPath

        cssPath ~> do
            S.writeFile' cssPath $ TL.unpack $ C.renderWith C.compact [] css

        "static-assets" ~> do
            assets <- S.getDirectoryFiles "." [ "static/assets//*" ]
            traverse_ copyStatic assets
