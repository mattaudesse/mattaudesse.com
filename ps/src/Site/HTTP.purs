module Site.HTTP
    ( postJson
    ) where

import Prelude
import Data.Either                  (Either(Left))
import Data.HTTP.Method             (Method(..))
import Data.Maybe                   (Maybe(..))
import Data.MediaType               (MediaType(..))
import Network.HTTP.Affjax          as AX
import Network.HTTP.Affjax.Request  as AXReq
import Network.HTTP.Affjax.Response as AXRes
import Network.HTTP.RequestHeader   as ReqH
import Simple.JSON                  (class WriteForeign, writeJSON)


applicationJson :: MediaType
applicationJson =  MediaType "application/json"


postJson :: forall a. WriteForeign a => AX.URL -> a -> AX.Affjax String
postJson url s = AX.affjax
    AXRes.string
    { method:          Left POST
    , headers:         [ ReqH.Accept      applicationJson
                       , ReqH.ContentType applicationJson
                       ]
    , content:         Just $ AXReq.string $ writeJSON s
    , username:        Nothing
    , password:        Nothing
    , withCredentials: false
    , url
    }
