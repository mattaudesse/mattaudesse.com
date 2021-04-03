module Site.HTTP
  ( post_
  ) where

import Prelude

import Affjax.StatusCode          (StatusCode)
import Data.Argonaut.Decode       (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode       (class EncodeJson, encodeJson)
import Data.Either                (Either(..))
import Data.HTTP.Method           (Method(POST))
import Data.Maybe                 (Maybe(..))
import Data.MediaType             (MediaType(..))
import Data.Time.Duration         (Milliseconds(..))
import Effect.Aff                 (Aff)

import Affjax                as AX
import Affjax.RequestBody    as ReqB
import Affjax.RequestHeader  as ReqH
import Affjax.ResponseHeader as ResH
import Affjax.ResponseFormat as AXRes


applicationJson :: MediaType
applicationJson =  MediaType "application/json"


type Response a =
  { status  :: { code :: StatusCode, text :: String }
  , headers :: Array  ResH.ResponseHeader
  , body    :: Either JsonDecodeError a
  }


mkReq
  :: forall a b
   . EncodeJson a
  => DecodeJson b
  => Method
  -> AX.URL
  -> a
  -> Aff (Either AX.Error (Response b))
mkReq meth url a = do
  resp <- AX.request
    { url
    , method:          Left meth
    , responseFormat:  AXRes.json
    , content:         Just $ ReqB.Json $ encodeJson a
    , username:        Nothing
    , password:        Nothing
    , withCredentials: false
    , timeout:         Just $ Milliseconds 120000.0
    , headers:         [ ReqH.Accept      applicationJson
                       , ReqH.ContentType applicationJson
                       ]
    }

  case resp of
    Left  e -> pure $ Left e
    Right r -> pure $ Right
      { status:  { code: r.status, text: r.statusText }
      , headers: r.headers
      , body:    decodeJson r.body
      }


post_
  :: forall a
   . EncodeJson a
  => AX.URL
  -> a
  -> Aff (Either AX.Error (Response Unit))
post_ = mkReq POST
