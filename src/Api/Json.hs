{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Json (JsonApi, jsonApi) where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API

type JsonApi =
  "api" :>
    (
      "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
      :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
      :<|> "users" :> Get  '[JSON] [User]
    )

jsonApi :: Proxy JsonApi
jsonApi = Proxy
