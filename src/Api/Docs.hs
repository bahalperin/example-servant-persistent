{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Docs (apiDocs) where

import Data.Text

import Database.Persist
import Database.Persist.Sql

import Models

import Servant.API
import Servant.Docs

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
