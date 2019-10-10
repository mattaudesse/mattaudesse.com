{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Site.Api.Contact
  ( Contact
  , VisitorMsgRequest(..)
  , VisitorMsgResponse(..)
  , VisitorMsgInvalid(..)
  , VisitorMsg
  , VisitorMsgId
  , VisitorIpAddr
  , contact
  , formatted
  , formattedWithRecordId
  , mkVisitorMsg
  , migrateVisitorMsg
  , visitorMsgsByCreationAsc
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader   (asks)
import Data.Aeson             (defaultOptions)
import Data.Aeson.TH          (deriveJSON)
import Data.Bifunctor         (bimap)
import Data.Char              (isSpace)
import Data.Either            (either)
import Data.Monoid            ((<>))
import Data.Text              (Text, pack, splitOn, strip)
import Data.Time.Clock        (UTCTime)
import Data.Validation        (Validation(..), toEither)
import Database.Persist.Sql   (Key, SqlPersistT, fromSqlKey)
import Database.Persist.TH    (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Safe                   (atMay)
import Servant                (Header, JSON, PostAccepted, ReqBody , err400, err403, (:>))

import qualified Data.Text        as T
import qualified Database.Persist as P

import Site.Spam (spamEmails, spamIps)
import Site.Core ( AppT
                 , Config(..)
                 , Email
                 , MonadAsync(..)
                 , MonadDB(..)
                 , MonadLog(..)
                 , MonadSmtp(..)
                 , MonadUTC(..)
                 , SmtpSendResult(..)
                 , VisitorIpAddr
                 , config
                 , waitAppT
                 , withBody
                 )


--------------------------------------------------------------------------------

type FirstName   = Text
type LastName    = Text
type Website     = Text
type Phone       = Text
type CommentBody = Text


data VisitorMsgRequest = VisitorMsgRequest
  { firstName   :: FirstName
  , lastName    :: LastName
  , email       :: Email
  , website     :: Maybe Website
  , phone       :: Maybe Phone
  , commentBody :: CommentBody
  } deriving (Eq, Show)


share [mkPersist sqlSettings, mkMigrate "migrateVisitorMsg"] [persistLowerCase|
  VisitorMsg
    firstName     FirstName
    lastName      LastName
    email         Email
    website       Website Maybe
    phone         Phone Maybe
    commentBody   CommentBody
    ipAddr        VisitorIpAddr
    utcTimestamp  UTCTime
    deriving      Eq Show
  |]


data VisitorMsgInvalid
  = MissingFirstName
  | MissingLastName
  | InvalidEmailWhitespace
  | MissingEmailAccount
  | MissingEmailAt
  | InvalidEmailMultipleAtSigns
  | MissingOrInvalidEmailDomain
  | MissingCommentBody
  deriving (Eq, Show)


data VisitorMsgResponse
  = VisitorMsgSuccess
  | VisitorMsgSpam
  | VisitorMsgInvalidMissingIp
  | VisitorMsgInvalidBecause [VisitorMsgInvalid]
  deriving (Eq, Show)


$(deriveJSON defaultOptions ''VisitorMsgRequest)
$(deriveJSON defaultOptions ''VisitorMsgInvalid)
$(deriveJSON defaultOptions ''VisitorMsgResponse)


--------------------------------------------------------------------------------

phoneOrDefault :: Maybe Phone -> Text
phoneOrDefault =  maybe "<no phone number provided>" id

websiteOrDefault :: Maybe Website -> Text
websiteOrDefault =  maybe "<no website provided>" id


--------------------------------------------------------------------------------

type ValidatorOf a = a -> Validation [VisitorMsgInvalid] a

trimLen :: Text -> Int
trimLen =  T.length . strip

missingFirstName :: ValidatorOf FirstName
missingFirstName n
  | trimLen n == 0 = Failure [MissingFirstName]
  | otherwise      = Success n

missingLastName :: ValidatorOf LastName
missingLastName n
  | trimLen n == 0 = Failure [MissingLastName]
  | otherwise      = Success n

invalidEmailWhitespace :: ValidatorOf Email
invalidEmailWhitespace e =
  let ws = T.filter isSpace e
   in if T.length ws > 0 then Failure [InvalidEmailWhitespace]
                         else Success e

missingEmailAt :: ValidatorOf Email
missingEmailAt e | "@" `T.isInfixOf` e = Success e
                 | otherwise           = Failure [MissingEmailAt]

missingEmailAccount :: ValidatorOf Email
missingEmailAccount e =
  let raw = flip atMay 0 $ splitOn "@" e
   in case raw of
      Nothing   -> Failure [MissingEmailAccount]
      Just raw' -> if trimLen raw' > 0
                      then Success e
                      else Failure [MissingEmailAccount]

missingOrInvalidEmailDomain :: ValidatorOf Email
missingOrInvalidEmailDomain e =
  let raw = flip atMay 1 $ splitOn "@" e
   in case raw of
      Nothing   -> Failure [MissingOrInvalidEmailDomain]
      Just raw' ->
        let parts = splitOn "." raw'
         in if length parts > 1 && all ((> 0) . trimLen) parts
               then Success e
               else Failure [MissingOrInvalidEmailDomain]

invalidEmailMultipleAtSigns :: ValidatorOf Email
invalidEmailMultipleAtSigns e =
  if T.count "@" e > 1 then Failure [InvalidEmailMultipleAtSigns]
                       else Success e

validEmail :: ValidatorOf Email
validEmail e =
  bimap id (const e) $ invalidEmailWhitespace      e
                    <* missingEmailAccount         e
                    <* missingEmailAt              e
                    <* invalidEmailMultipleAtSigns e
                    <* missingOrInvalidEmailDomain e

missingCommentBody :: ValidatorOf CommentBody
missingCommentBody b
  | trimLen b == 0 = Failure [MissingCommentBody]
  | otherwise      = Success b


mkVisitorMsg :: VisitorIpAddr
             -> UTCTime
             -> VisitorMsgRequest
             -> Either [VisitorMsgInvalid] VisitorMsg
mkVisitorMsg ip utc VisitorMsgRequest {..} = do
  toEither $ VisitorMsg <$> missingFirstName   firstName
                        <*> missingLastName    lastName
                        <*> validEmail         email
                        <*> Success            website
                        <*> Success            phone
                        <*> missingCommentBody commentBody
                        <*> Success            ip
                        <*> Success            utc


--------------------------------------------------------------------------------

formatted :: VisitorMsg -> Text
formatted VisitorMsg {..} = msg
  where nameWithEmail = visitorMsgFirstName
                     <> " "
                     <> visitorMsgLastName
                     <> " ("
                     <> visitorMsgEmail
                     <> ")"

        msg =    "At "                <> (pack $ show visitorMsgUtcTimestamp)
           <> "\n"                    <> nameWithEmail
           <> "\nfrom IP:           " <> visitorMsgIpAddr
           <> "\nand website:       " <> websiteOrDefault visitorMsgWebsite
           <> "\nwith phone number: " <> phoneOrDefault   visitorMsgPhone
           <> "\nsaid:"
           <> "\n\n"
           <> visitorMsgCommentBody
           <> "\n\n"


formattedWithRecordId :: VisitorMsg -> Key VisitorMsg -> Text
formattedWithRecordId vm k = "Record ID: "
                          <> (pack . show . fromSqlKey) k
                          <> "\n"
                          <> formatted vm


--------------------------------------------------------------------------------

type Contact = "contact"
            :> Header "X-Forwarded-For" VisitorIpAddr
            :> ReqBody      '[JSON] VisitorMsgRequest
            :> PostAccepted '[JSON] VisitorMsgResponse


process :: ( MonadAsync (AppT m)
           , MonadDB    (AppT m)
           , MonadLog   (AppT m)
           , MonadSmtp  (AppT m)
           , Monad m
           )
           => VisitorMsg
           -> AppT m VisitorMsgResponse
process vm = do
  let subject    = "Visitor message from mattaudesse.com"
      logMsg v k = pack (replicate 50 '-') <> "\n" <> formattedWithRecordId v k

  Config { smtpSender } <- asks config

  emailed    <- runAsync (sendEmail smtpSender subject $ formatted vm)
  inserted   <- runAsync (runDb $ P.insert vm)
  recordId   <- waitAppT inserted
  info        $ logMsg   vm recordId
  smtpResult <- waitAppT emailed

  case smtpResult of
    SmtpSendFailedToAuthenticate ->
      crit "Failed to authenticate with mail server!"

    SmtpSendSuccess ->
      info ("Visitor message email sent for record ID: "
        <> (pack $ show $ fromSqlKey recordId))

  return VisitorMsgSuccess


contact :: ( MonadAsync (AppT m)
           , MonadDB    (AppT m)
           , MonadLog   (AppT m)
           , MonadSmtp  (AppT m)
           , MonadUTC   (AppT m)
           , MonadIO m
           )
        => Maybe VisitorIpAddr
        -> VisitorMsgRequest
        -> AppT m VisitorMsgResponse
contact Nothing   _   = err400 `withBody` VisitorMsgInvalidMissingIp
contact (Just ip) req = do
  utc <- utcNow

  let req'          = mkVisitorMsg ip utc req
      invalid400 es = err400 `withBody` VisitorMsgInvalidBecause es
      spam403       = err403 `withBody` VisitorMsgSpam

      spamOrAccept vm@VisitorMsg {..}
        | visitorMsgEmail  `elem` spamEmails = spam403
        | visitorMsgIpAddr `elem` spamIps    = spam403
        | otherwise                          = process vm

  either invalid400 spamOrAccept req'


--------------------------------------------------------------------------------

visitorMsgsByCreationAsc :: MonadIO m => SqlPersistT m [P.Entity VisitorMsg]
visitorMsgsByCreationAsc =
  P.selectList ([] :: [P.Filter VisitorMsg])
               ([P.Asc VisitorMsgUtcTimestamp])
