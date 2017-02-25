
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.StaticFiles (StaticFilesApi) where

import Data.Proxy
import Data.Text

import Models

import Servant.API
import Servant.Utils.StaticFiles
import Servant.HTML.Blaze

import Text.Blaze.Html5 (Html)

type StaticFilesApi =
  Get '[HTML] Html
  :<|> "static" :> Raw
