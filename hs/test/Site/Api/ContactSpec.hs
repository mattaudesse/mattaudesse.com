{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Site.Api.ContactSpec (contactHandler, visitorMsgValidation) where

import Data.Aeson         (encode)
import Data.Either        (isRight)
import Data.List.NonEmpty as NE
import Data.Text          (pack)
import NeatInterpolation  (text)
import Servant            (ServerError(..), err400, err403)
import Test.Hspec         (Spec, describe, it, shouldBe, shouldContain)

import Site.Core  (Env)
import Site.Spam  (spamEmails, spamIps)
import SpecCommon (SideEffect(..), dummyUtc, localhost, effectsFrom, resultOf)

import Site.Api.Contact ( VisitorMsgRequest(..)
                        , VisitorMsgInvalid(..)
                        , VisitorMsgResponse(..)
                        , VisitorMsg
                        , VisitorIpAddr
                        , contact
                        , mkVisitorMsg
                        )


--------------------------------------------------------------------------------

joeTestMsgReq :: VisitorMsgRequest
joeTestMsgReq =  VisitorMsgRequest
  { firstName   = "Joe"
  , lastName    = "Test"
  , email       = "joe@example.com"
  , website     = Just "www.example.com"
  , phone       = Just "123-456-7890 ext. 311"
  , commentBody = "hi"
  }


mkVisitorMsg' :: VisitorMsgRequest -> Either [VisitorMsgInvalid] VisitorMsg
mkVisitorMsg' =  mkVisitorMsg ("127.0.0.1" :: VisitorIpAddr) dummyUtc


--------------------------------------------------------------------------------

visitorMsgValidation :: Spec
visitorMsgValidation =  describe "`VisitorMsgRequest` validation ensures that" $ do
  let whitespace = "   \n \t "

  describe "`firstName` fields" $ do
    it "which are empty fail" $
      mkVisitorMsg' joeTestMsgReq { firstName = "" }
        `shouldBe` Left [MissingFirstName]

    it "aren't fooled by whitespace" $ do
      mkVisitorMsg' joeTestMsgReq { firstName = whitespace }
        `shouldBe` Left [MissingFirstName]


  describe "`lastName` fields" $ do
    it "which are empty fail" $
      mkVisitorMsg' joeTestMsgReq { lastName = whitespace }
        `shouldBe` Left [MissingLastName]

    it "aren't fooled by whitespace" $ do
      mkVisitorMsg' joeTestMsgReq { lastName = whitespace }
        `shouldBe` Left [MissingLastName]


  describe "`email` fields" $ do
    it "which are empty fail checks for missing account, `@`, and domain" $
      mkVisitorMsg' joeTestMsgReq { email = "" }
        `shouldBe` Left [ MissingEmailAccount
                        , MissingEmailAt
                        , MissingOrInvalidEmailDomain
                        ]

    it "aren't fooled by whitespace" $ do
      mkVisitorMsg' joeTestMsgReq { firstName = whitespace }
        `shouldBe` Left [MissingFirstName]

    it "which lack an account fail" $ do
      mkVisitorMsg' joeTestMsgReq { email = "@example.com" }
        `shouldBe` Left [MissingEmailAccount]

      mkVisitorMsg' joeTestMsgReq { email = " @example.com" }
        `shouldBe` Left [ InvalidEmailWhitespace
                        , MissingEmailAccount
                        ]

    it "which lack an `@` character fail" $
      mkVisitorMsg' joeTestMsgReq { email = "nope-at-example.com" }
        `shouldBe` Left [ MissingEmailAt
                        , MissingOrInvalidEmailDomain
                        ]

    it "which have more than one `@` character fail" $
      mkVisitorMsg' joeTestMsgReq { email = "joe@bork.tld@example.com" }
        `shouldBe` Left [InvalidEmailMultipleAtSigns]

    it "which lack a domain fail" $ do
      mkVisitorMsg' joeTestMsgReq { email = "joe@" }
        `shouldBe` Left [MissingOrInvalidEmailDomain]

      mkVisitorMsg' joeTestMsgReq { email = "joe@ " }
        `shouldBe` Left [ InvalidEmailWhitespace
                        , MissingOrInvalidEmailDomain
                        ]

    it "require a domain of form: <one or more subdomains>.<tld>" $ do
      mkVisitorMsg' joeTestMsgReq { email = "joe@foo" }
        `shouldBe` Left [MissingOrInvalidEmailDomain]

      mkVisitorMsg' joeTestMsgReq { email = "joe@bar." }
        `shouldBe` Left [MissingOrInvalidEmailDomain]

      mkVisitorMsg' joeTestMsgReq { email = "joe@.tld" }
        `shouldBe` Left [MissingOrInvalidEmailDomain]

      mkVisitorMsg' joeTestMsgReq { email = "joe@one.two.three." }
        `shouldBe` Left [MissingOrInvalidEmailDomain]

      mkVisitorMsg' joeTestMsgReq { email = "joe@a. ..d" }
        `shouldBe` Left [ InvalidEmailWhitespace
                        , MissingOrInvalidEmailDomain
                        ]

      isRight (mkVisitorMsg' joeTestMsgReq { email = "joe@a.b.c.d" })
        `shouldBe` True


  describe "`commentBody` fields" $ do
    it "which are empty fail" $
      mkVisitorMsg' joeTestMsgReq { commentBody = "" }
        `shouldBe` Left [MissingCommentBody]

    it "aren't fooled by whitespace" $ do
      mkVisitorMsg' joeTestMsgReq { commentBody = whitespace }
        `shouldBe` Left [MissingCommentBody]


  describe "the full set of errors is reported" $ do
    it "when multiple fields are invalid" $
      mkVisitorMsg' joeTestMsgReq { firstName   = ""
                                  , email       = "nope"
                                  , commentBody = ""
                                  }
        `shouldBe` Left [ MissingFirstName
                        , MissingEmailAt
                        , MissingOrInvalidEmailDomain
                        , MissingCommentBody
                        ]


  describe "well-formed contents" $
    it "pass through to `VisitorMsg` constructor" $
      isRight (mkVisitorMsg' joeTestMsgReq) `shouldBe` True


--------------------------------------------------------------------------------

contactHandler :: Env -> Spec
contactHandler env = describe "POST /api/contact" $ do

  describe "without an X-Forwarded-For header" $
    it "raises HTTP 400 with `VisitorMsgInvalidMissingIp` payload" $ do
      resp <- resultOf env (contact Nothing joeTestMsgReq)
      resp `shouldBe` Left (err400 { errBody = encode VisitorMsgInvalidMissingIp })


  describe "from a spammy X-Forwarded-For IP" $ do
    it "raises HTTP 403 with `VisitorMsgSpam` payload" $ do
      resp <- resultOf env (contact (Just (NE.head spamIps)) joeTestMsgReq)
      resp `shouldBe` Left (err403 { errBody = encode VisitorMsgSpam })


  describe "with a spammy email field" $ do
    it "raises HTTP 403 with `VisitorMsgSpam` payload" $ do
      resp <- resultOf env (contact (Just localhost)
                                    (joeTestMsgReq { email = NE.head spamEmails }))
      resp `shouldBe` Left (err403 { errBody = encode VisitorMsgSpam })


  describe "with malformed contents" $
    it "raises HTTP 400 with `VisitorMsgInvalidBecause es` payload" $ do
      let req = joeTestMsgReq { firstName   = ""
                              , email       = "this-is-invalid"
                              , commentBody = ""
                              }
      resp <- resultOf env (contact (Just localhost) req)

      resp `shouldBe` Left (err400 { errBody =
        encode $ VisitorMsgInvalidBecause [ MissingFirstName
                                          , MissingEmailAt
                                          , MissingOrInvalidEmailDomain
                                          , MissingCommentBody
                                          ] })


  describe "with well-formed contents" $ do
    let header     = pack (replicate 50 '-' <> "\n")
        recordLine = pack "Record ID: 1\n"
        msg        = [text|At 2018-01-01 00:00:00 UTC
                           Joe Test (joe@example.com)
                           from IP:           127.0.0.1
                           and website:       www.example.com
                           with phone number: 123-456-7890 ext. 311
                           said:

                           hi
                          |] <> (pack "\n")

    it "logs the message + associated metadata" $ do
      effs <- effectsFrom env (contact (Just localhost) joeTestMsgReq)
      effs `shouldContain` [LogInfo (header <> recordLine <> msg)]


    it "asynchronously emails the message + associated metadata to me" $ do
      effs <- effectsFrom env (contact (Just localhost) joeTestMsgReq)
      effs `shouldContain` [Async [Smtp "smtp-sender-email-addr"
                                        "Visitor message from mattaudesse.com"
                                        msg]]
      effs `shouldContain` [LogInfo "Visitor message email sent for record ID: 1"]


    it "asynchronously stashes the message + associated metadata to database" $ do
      effs <- effectsFrom env (contact (Just localhost) joeTestMsgReq)
      effs `shouldContain` [Async [DbQuery]]
