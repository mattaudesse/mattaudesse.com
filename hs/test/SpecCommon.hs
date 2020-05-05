{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
module SpecCommon
  ( TestM(..)
  , SideEffect(..)
  , dummyUtc
  , effectsFrom
  , getWith
  , localhost
  , resultOf
  ) where

import Control.Concurrent.Async (async)
import Control.Exception        (throw)
import Control.Monad.Except     (ExceptT(..), runExceptT)
import Control.Monad.IO.Class   (MonadIO(..))
import Control.Monad.Reader     (ReaderT(..), runReaderT)
import Control.Monad.Writer     (MonadWriter(..), WriterT(..), execWriterT)
import Data.ByteString          (ByteString)
import Data.Text                (Text)
import Data.Time.Calendar       (fromGregorian)
import Data.Time.Clock          (UTCTime(..), secondsToDiffTime)
import Data.Time.Format         (defaultTimeLocale, formatTime)
import Database.Persist.Sql     (runSqlPool)
import Network.HTTP.Types       (Header, methodGet)
import Network.Wai.Test         (SResponse)
import Servant                  (ServerError(..))
import Test.Hspec.Wai           (WaiSession, request)

import Site.Api.Contact (VisitorIpAddr)
import Site.Core
  ( AppT(..)
  , Env(..)
  , EmailRecipient
  , EmailSubject
  , MonadAsync(..)
  , MonadDB(..)
  , MonadLog(..)
  , MonadSmtp(..)
  , MonadUTC(..)
  , SmtpSendResult(..)
  )


--------------------------------------------------------------------------------

dummyUtc :: UTCTime
dummyUtc =  UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0)

localhost :: VisitorIpAddr
localhost =  "127.0.0.1"


--------------------------------------------------------------------------------

data SideEffect
  = Async   [SideEffect]
  | LogInfo Text
  | LogCrit Text
  | Smtp    EmailRecipient EmailSubject Text
  | DbQuery
  | Utc
    deriving (Eq, Show)


--------------------------------------------------------------------------------

newtype TestM a = TestM (WriterT [SideEffect] IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter [SideEffect]
           , MonadIO
           )


instance MonadLog TestM where
  info msg = tell [LogInfo msg]
  crit msg = tell [LogCrit msg]


instance MonadUTC TestM where
  utcNow         = tell [Utc] >>  pure dummyUtc
  utcCurrentYear = utcNow     >>= pure . formatTime defaultTimeLocale "%Y"


instance MonadSmtp TestM where
  sendEmail recipient subject body = do
    tell [Smtp recipient subject body]
    pure SmtpSendSuccess


instance MonadSmtp (AppT TestM) where
  sendEmail recipient subject body =
    AppT $ ReaderT (\_ -> sendEmail recipient subject body)


instance MonadDB (AppT TestM) where
  runDb query = AppT $ ReaderT $ \env -> ExceptT $ do
    tell [DbQuery]
    r <- liftIO $ runSqlPool query (dbPool env)
    pure $ Right r


-- Simulate concurrency
instance MonadAsync (AppT TestM) where
  runAsync a = AppT $ ReaderT $ \env -> ExceptT $ do
    let f (TestM w) = liftIO $ runWriterT w

    (r, effs) <- f $ runAppTestM env a
    tell [Async effs]

    case r of
      Left  e  -> throw e
      Right r' -> do
        r'' <- liftIO $ async (pure r')
        pure $ Right r''


--------------------------------------------------------------------------------

runAppTestM :: Env -> AppT TestM a -> TestM (Either ServerError a)
runAppTestM env a = runExceptT $ runReaderT (runApp a) env


effectsFrom :: Env -> AppT TestM a -> IO [SideEffect]
effectsFrom env =
  let logTestM (TestM w) = execWriterT w
   in logTestM . (runAppTestM env)


resultOf :: Env -> AppT TestM a -> IO (Either ServerError a)
resultOf env =
  let evalTestM (TestM w) = runWriterT w >>= pure . fst
   in evalTestM . (runAppTestM env)


--------------------------------------------------------------------------------

getWith :: [Header] -> ByteString -> WaiSession st SResponse
getWith hdrs path = request methodGet path hdrs ""
