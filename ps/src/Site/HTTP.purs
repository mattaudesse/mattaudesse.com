module Site.HTTP (postJson) where

import Prelude (($))

import Affjax.RequestBody (string)
import Data.Either        (Either(Left))
import Data.HTTP.Method   (Method(POST))
import Data.Maybe         (Maybe(..))
import Data.MediaType     (MediaType(..))
import Effect.Aff         (Aff)
import Simple.JSON        (class WriteForeign, writeJSON)

import Affjax                as AX
import Affjax.RequestHeader  as ReqH
import Affjax.ResponseFormat as AXRes
import Data.Argonaut.Core    as Argo


applicationJson :: MediaType
applicationJson =  MediaType "application/json"


postJson
  :: forall a
   . WriteForeign a
  => AX.URL
  -> a
  -> Aff (AX.Response (Either AX.ResponseFormatError Argo.Json))
postJson url s = AX.request
  { method:          Left POST
  , headers:         [ ReqH.Accept      applicationJson
                     , ReqH.ContentType applicationJson
                     ]
  , responseFormat:  AXRes.json
  , content:         Just $ string $ writeJSON s
  , username:        Nothing
  , password:        Nothing
  , withCredentials: false
  , url
  }
