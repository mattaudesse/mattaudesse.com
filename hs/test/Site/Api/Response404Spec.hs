{-# LANGUAGE OverloadedStrings #-}
module Site.Api.Response404Spec (response404Handler) where

import Test.Hspec     (Spec, describe, it)
import Test.Hspec.Wai ((<:>), get, matchHeaders, matchStatus, with, shouldRespondWith)
import Site           (siteApp)
import SpecCommon     (getWith)

import qualified Site.Core as C


response404Handler :: C.Env -> Spec
response404Handler env = with (return (siteApp env)) $ do
  let resCTAJ = fst C.contentTypeApplicationJson
            <:> snd C.contentTypeApplicationJson

      resCTTH = fst C.contentTypeTextHtml
            <:> snd C.contentTypeTextHtml


  describe "GET /a/nonexistent/route" $ do
    it "responds with status 404" $
      get "/this/does/not/exist" `shouldRespondWith` 404

    it "responds with `Content-Type: text/html` header" $
      get "/no/such/thing"
        `shouldRespondWith` 404 { matchHeaders = [resCTTH] }


    describe "with `Accept: application/json` request header" $ do
      it "responds with status 404" $
        getWith [C.acceptApplicationJson] "/this/does/not/exist"
          `shouldRespondWith` 404

      it "responds with body payload `\"\"`" $ do
        getWith [C.acceptApplicationJson] "/api/yo/dawg"
          `shouldRespondWith` "" { matchStatus = 404 }

      it "responds with `Content-Type: application/json` header" $
        getWith [C.acceptApplicationJson] "/no/such/thing"
          `shouldRespondWith` 404 { matchHeaders = [resCTAJ] }


    describe "with `Content-Type: application/json` request header" $ do
      it "responds with status 404" $
        getWith [C.contentTypeApplicationJson] "/this/does/not/exist"
          `shouldRespondWith` 404

      it "responds with body payload `\"\"`" $ do
        getWith [C.contentTypeApplicationJson] "/api/yo/dawg"
          `shouldRespondWith` "" { matchStatus = 404 }

      it "responds with `Content-Type: application/json` header" $
        getWith [C.contentTypeApplicationJson] "/no/such/thing"
          `shouldRespondWith` 404 { matchHeaders = [resCTAJ] }
