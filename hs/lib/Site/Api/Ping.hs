{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Site.Api.Ping (Ping , ping) where

import Data.Aeson    (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant       (Get, JSON, (:>))
import Site.Core     (AppT)


newtype Pong = Pong
    { pong :: String
    } deriving (Show)

$(deriveJSON defaultOptions ''Pong)

pong' :: Pong
pong' =  Pong { pong = "pong" }


type Ping = "ping" :> Get '[JSON] Pong

ping :: Monad m => AppT m Pong
ping =  return pong'
