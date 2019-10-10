{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Site.Api
  ( Api
  , apiServerFrom
  ) where

import Control.Monad.Reader (runReaderT)
import Servant              ((:<|>)(..))
import Site.Api.Contact     (Contact, contact)
import Site.Api.Ping        (Ping, ping)
import Site.Core            (AppT, Env, runApp)

import qualified Servant as S


type Api = Ping :<|> Contact


apiServerFrom :: Env -> S.Server Api
apiServerFrom env =
  S.hoistServer proxy toHandler routes

  where toHandler :: AppT IO a -> S.Handler a
        toHandler a = S.Handler $ runReaderT (runApp a) env

        proxy :: S.Proxy Api
        proxy =  S.Proxy

        routes = ping :<|> contact
