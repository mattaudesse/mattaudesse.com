module Site.Api.ContactSpec (contactValidation) where

import Prelude
import Data.Array.NonEmpty       (toArray)
import Data.Either               (Either(..))
import Data.Maybe                (Maybe(..))
import Data.Validation.Semigroup (toEither)
import Test.Spec                 (Spec, describe, it)
import Test.Spec.Assertions      (shouldEqual)

import Site.Api.Contact as C


joeTestMsgReq :: C.Request
joeTestMsgReq =
    { firstName:   "Joe"
    , lastName:    "Test"
    , email:       "joe@example.com"
    , website:     Just "www.example.com"
    , phone:       Just "123-456-7890 ext. 311"
    , commentBody: "hi"
    }

validated :: C.Request -> Either (Array C.VisitorMsgInvalid) C.Request
validated r = case (toEither $ C.validRequest r) of
    Right r' -> Right r'
    Left  es -> Left  $ toArray es

whitespace :: String
whitespace =  "    \n \t "


contactValidation :: Spec Unit
contactValidation =  describe "Contact form `Request` validation ensures that" $ do

    describe "`firstName` fields" $ do
        it "which are empty fail" $
            validated (joeTestMsgReq { firstName = "" })
                `shouldEqual` Left [C.MissingFirstName]

        it "aren't fooled by whitespace" $
            validated (joeTestMsgReq { firstName = whitespace })
                `shouldEqual` Left [C.MissingFirstName]


    describe "`lastName` fields" $ do
        it "which are empty fail" $
            validated (joeTestMsgReq { lastName = "" })
                `shouldEqual` Left [C.MissingLastName]

        it "aren't fooled by whitespace" $
            validated (joeTestMsgReq { lastName = whitespace })
                `shouldEqual` Left [C.MissingLastName]


    describe "`email` fields" $ do
        it "which are empty fail checks for missing account, `@`, and domain" $
            validated (joeTestMsgReq { email = "" })
                `shouldEqual` Left [ C.MissingEmailAccount
                                   , C.MissingEmailAt
                                   , C.MissingOrInvalidEmailDomain
                                   ]

        it "aren't fooled by whitespace" $
            validated (joeTestMsgReq { email = whitespace })
                `shouldEqual` Left [ C.InvalidEmailWhitespace
                                   , C.MissingEmailAccount
                                   , C.MissingEmailAt
                                   , C.MissingOrInvalidEmailDomain
                                   ]

        it "which lack an account fail" $ do
            validated (joeTestMsgReq { email = "@example.com" })
                `shouldEqual` Left [C.MissingEmailAccount]

            validated (joeTestMsgReq { email = " @example.com" })
                `shouldEqual` Left [ C.InvalidEmailWhitespace
                                   , C.MissingEmailAccount
                                   ]

        it "which lack an `@` character fail" $
            validated (joeTestMsgReq { email = "nope-at-example.com" })
                `shouldEqual` Left [ C.MissingEmailAt
                                   , C.MissingOrInvalidEmailDomain
                                   ]

        it "which have more than one `@` character fail" $
            validated (joeTestMsgReq { email = "joe@bork.tld@example.com" })
                `shouldEqual` Left [C.InvalidEmailMultipleAtSigns]

        it "which lack a domain fail" $ do
            validated (joeTestMsgReq { email = "joe@" })
                `shouldEqual` Left [C.MissingOrInvalidEmailDomain]

            validated (joeTestMsgReq { email = "joe@ " })
                `shouldEqual` Left [ C.InvalidEmailWhitespace
                                   , C.MissingOrInvalidEmailDomain
                                   ]

        it "require a domain of form: <one or more subdomains>.<tld>" $ do
            validated (joeTestMsgReq { email = "joe@foo" })
                `shouldEqual` Left [C.MissingOrInvalidEmailDomain]

            validated (joeTestMsgReq { email = "joe@bar." })
                `shouldEqual` Left [C.MissingOrInvalidEmailDomain]

            validated (joeTestMsgReq { email = "joe@.tld" })
                `shouldEqual` Left [C.MissingOrInvalidEmailDomain]

            validated (joeTestMsgReq { email = "joe@one.two.three." })
                `shouldEqual` Left [C.MissingOrInvalidEmailDomain]

            validated (joeTestMsgReq { email = "joe@a. ..d" })
                `shouldEqual` Left [ C.InvalidEmailWhitespace
                                   , C.MissingOrInvalidEmailDomain
                                   ]

            validated (joeTestMsgReq { email = "joe@a.b.c.d" })
                `shouldEqual` Right (joeTestMsgReq { email = "joe@a.b.c.d" })


    describe "`commentBody` fields" $ do
        it "which are empty fail" $
            validated (joeTestMsgReq { commentBody = "" })
                `shouldEqual` Left [C.MissingCommentBody]

        it "aren't fooled by whitespace" $
            validated (joeTestMsgReq { commentBody = whitespace })
                `shouldEqual` Left [C.MissingCommentBody]


    describe "the full set of errors is reported" $
        it "when multiple fields are invalid" $
            validated (joeTestMsgReq { firstName   = ""
                                     , email       = "nope"
                                     , commentBody = ""
                                     })
                `shouldEqual` Left [ C.MissingFirstName
                                   , C.MissingEmailAt
                                   , C.MissingOrInvalidEmailDomain
                                   , C.MissingCommentBody
                                   ]


    describe "well-formed contents" $
        it "pass validation" $ validated joeTestMsgReq
            `shouldEqual` Right joeTestMsgReq
