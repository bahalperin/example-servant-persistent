{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, api) where

import Data.Proxy

import Servant.API

import Api.Json (JsonApi, jsonApi)
import Api.StaticFiles (StaticFilesApi)
import Api.Docs (apiDocs)

type Api =
  JsonApi
  :<|> StaticFilesApi
  :<|> Raw

api :: Proxy Api
api = Proxy
