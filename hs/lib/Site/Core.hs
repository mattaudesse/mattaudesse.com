{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Site.Core
  ( AppT(..)
  , Config(..)
  , Email
  , EmailRecipient
  , EmailSubject
  , Env(..)
  , MonadAsync(..)
  , MonadDB(..)
  , MonadLog(..)
  , MonadSmtp(..)
  , MonadUTC(..)
  , SmtpHostName
  , SmtpSendResult(..)
  , VisitorIpAddr
  , acceptApplicationJson
  , contentTypeApplicationJson
  , contentTypeTextHtml
  , waitAppT
  , withBody
  ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Exception        (catch, throw)
import Control.Monad.Except     (ExceptT(..), MonadError, throwError, runExceptT)
import Control.Monad.IO.Class   (MonadIO(..))
import Control.Monad.Logger     (logErrorN, logInfoN, runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Reader     (MonadReader, ReaderT(..), asks)
import Data.Aeson               (ToJSON, encode, defaultOptions)
import Data.Aeson.TH            (deriveJSON)
import Data.Text                (Text)
import Data.Time.Clock          (UTCTime, getCurrentTime)
import Data.Time.Format         (defaultTimeLocale, formatTime)
import Database.Persist.Sql     (SqlPersistT, runSqlPool)
import Database.Persist.Sqlite  (ConnectionPool)
import GHC.IO.Exception         (IOException(..))
import Network.HTTP.Types       (Header, hAccept, hContentType)
import Network.Mail.Mime        (plainPart)
import Servant                  (ServerError, errBody)

import qualified Data.Text.Lazy    as DTL
import qualified Network.Mail.SMTP as SMTP


--------------------------------------------------------------------------------

type SmtpHostName   = String
type Email          = Text
type EmailSender    = Email
type EmailRecipient = Email
type EmailSubject   = Text
type VisitorIpAddr  = Text


data Config = Config
  { port         :: Int
  , smtpHostName :: SmtpHostName
  , smtpUserName :: SMTP.UserName
  , smtpPassword :: SMTP.Password
  , smtpSender   :: EmailSender
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Config)


data Env = Env
  { dbPool :: ConnectionPool
  , config :: Config
  }


newtype AppT m a = AppT
  { runApp :: ReaderT Env (ExceptT ServerError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadError  ServerError
             , MonadIO
             )


--------------------------------------------------------------------------------

class Monad m => MonadLog m where
  info :: Text -> m ()
  crit :: Text -> m ()

instance MonadLog IO where
  info = runStdoutLoggingT . logInfoN
  crit = runStderrLoggingT . logErrorN

instance MonadLog m => MonadLog (ExceptT ServerError m) where
  info a = ExceptT $ info a >>= pure . Right
  crit a = ExceptT $ crit a >>= pure . Right

instance MonadLog m => MonadLog (AppT m) where
  info a = AppT $ ReaderT (\_ -> info a)
  crit a = AppT $ ReaderT (\_ -> crit a)


--------------------------------------------------------------------------------

class Monad m => MonadUTC m where
  utcNow         :: m UTCTime
  utcCurrentYear :: m String

instance MonadUTC IO where
  utcNow         = getCurrentTime
  utcCurrentYear = utcNow >>= pure . formatTime defaultTimeLocale "%Y"

instance MonadUTC m => MonadUTC (ExceptT ServerError m) where
  utcNow         = ExceptT $ utcNow         >>= pure . Right
  utcCurrentYear = ExceptT $ utcCurrentYear >>= pure . Right

instance MonadUTC m => MonadUTC (AppT m) where
  utcNow         = AppT $ ReaderT (const utcNow)
  utcCurrentYear = AppT $ ReaderT (const utcCurrentYear)


--------------------------------------------------------------------------------

class MonadDB m where
  runDb :: SqlPersistT IO b -> m b

instance MonadDB (AppT IO) where
  runDb query = asks dbPool >>= liftIO . runSqlPool query


--------------------------------------------------------------------------------

data SmtpSendResult
  = SmtpSendSuccess
  | SmtpProtoFailure


class Monad m => MonadSmtp m where
  sendEmail :: EmailRecipient
            -> EmailSubject
            -> Text
            -> m SmtpSendResult

instance MonadSmtp (AppT IO) where
  sendEmail recipient subject body = do
    Config {..} <- asks config
    let addr = SMTP.Address Nothing
        proc = SMTP.sendMailWithLoginTLS
            smtpHostName
            smtpUserName
            smtpPassword
          $ SMTP.simpleMail
            ( addr smtpSender )
            [ addr recipient  ]
            [] -- CC
            [] -- BCC
            subject
            [ plainPart $ DTL.fromStrict body ]

    liftIO $ (proc >> pure SmtpSendSuccess)
      `catch` \IOError {} -> pure SmtpProtoFailure


instance MonadSmtp m => MonadSmtp (ExceptT ServerError m) where
  sendEmail recipient subject body =
    ExceptT $ Right <$> sendEmail recipient subject body


--------------------------------------------------------------------------------

class MonadIO m => MonadAsync m where
  runAsync :: m a -> m (Async a)

instance MonadAsync (AppT IO) where
  runAsync a = AppT $ ReaderT $ \conf ->
    liftIO $ async $ do
      r <- runExceptT $ runReaderT (runApp a) conf
      either throw pure r

-- | Await completion of a thread spawned by `runAsync`
waitAppT :: MonadIO m => Async a -> m a
waitAppT =  liftIO . wait


--------------------------------------------------------------------------------

-- | Customize a `ServerError` having `s` status with `b` body contents
withBody :: (ToJSON b, Monad m) => ServerError -> b -> AppT m a
withBody s b = throwError (s { errBody = encode b })


--------------------------------------------------------------------------------

acceptApplicationJson :: Header
acceptApplicationJson =  (hAccept, "application/json")

contentTypeApplicationJson :: Header
contentTypeApplicationJson =  (hContentType, "application/json")

contentTypeTextHtml :: Header
contentTypeTextHtml =  (hContentType, "text/html")
