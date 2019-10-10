{-# LANGUAGE OverloadedStrings #-}
module Site.Api.PingSpec (pingHandler) where

import Test.Hspec     (Spec, describe, it)
import Test.Hspec.Wai (get, with, shouldRespondWith)
import Site           (siteApp)
import Site.Core      (Env)

pingHandler :: Env -> Spec
pingHandler env = with (return (siteApp env)) $ do

  describe "GET /api/ping" $ do
    it "responds with status 200" $
      get "/api/ping" `shouldRespondWith` 200

    it "responds with body payload `{pong: \"pong\"}`" $
      get "/api/ping" `shouldRespondWith` "{\"pong\":\"pong\"}"
