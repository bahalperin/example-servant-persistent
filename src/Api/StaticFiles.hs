
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.StaticFiles (StaticFilesApi, staticFilesServer) where

import           Control.Monad.IO.Class

import qualified Data.ByteString as BS

import Data.Proxy
import Data.Text
import Text.Blaze

import Models

import Servant
import Servant.API
import Servant.Utils.StaticFiles
import Servant.HTML.Blaze

import Text.Blaze.Html5 (Html)

type StaticFilesApi =
  Get '[HTML] Html
  :<|> "static" :> Raw


staticFilesServer :: Server StaticFilesApi
staticFilesServer =
  fmap unsafeByteString (liftIO $ BS.readFile "static/index.html") :<|> serveDirectory "static"
