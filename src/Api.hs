{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, api) where

import           Data.Proxy

import           Servant.API

import           Api.Docs        (apiDocs)
import           Api.Json        (JsonApi, jsonApi)
import           Api.StaticFiles (StaticFilesApi)

type Api =
  JsonApi
  :<|> StaticFilesApi
  :<|> Raw

api :: Proxy Api
api = Proxy
