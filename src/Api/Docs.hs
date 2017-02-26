{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Docs (apiDocs, serveDocs) where

import Data.Text
import qualified Data.Text.Lazy (pack)

import qualified Data.ByteString.Lazy as BSL
import Database.Persist
import Database.Persist.Sql

import Models

import Servant
import Servant.API
import Servant.Docs

import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Network.HTTP.Types         as HTTP
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import Api.Json (jsonApi, JsonApi)

instance ToCapture (Capture "name" Text) where
  toCapture _ =
    DocCapture "name" "(String) The name of the user"

instance ToSample User where
  toSamples _ =
    [ ("exampleUser", User "exampleUser" 23)
    , ("anotherUser", User "anotherUser" 55)
    ]

instance {-# OVERLAPS #-} ToSample [User] where
  toSamples _ =
    samples
      [
        [ User "exampleUser" 23
        , User "anotherUser" 55
        , User "yetAnotherUser" 34
        , User "theLastUser" 47
        ]
      ]

instance ToSample (Key User) where
  toSamples _ =
    [ ("If the user is new and is added to the database", toSqlKey 1)
    ]

apiDocs ::  API
apiDocs = docs jsonApi

serveDocs :: Application
serveDocs _ respond =
    respond $ responseLBS HTTP.ok200 [plain] docsBS


docsBS :: BSL.ByteString
docsBS = encodeUtf8
       . Data.Text.Lazy.pack
       . markdown
       $ docsWithIntros [intro] jsonApi
  where intro = DocIntro "Servant Example App" ["This is the documentation for the Servant Example App API.", "Enjoy!"]

plain = ("Content-Type", "text/plain")
