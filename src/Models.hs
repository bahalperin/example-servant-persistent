{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import qualified Control.Applicative as CA (pure, empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text
import qualified Data.Text.Lazy as TL ( fromStrict, unpack )
import qualified Data.HashMap.Lazy (lookup)

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age  Int
  UniqueName name
  deriving Eq Read Show
|]


instance FromJSON User where
  parseJSON = withObject "User" $ \ object -> do
    name <- object .: "name"
    age <- object .: "age"
    return $ User name age

instance ToJSON User where
  toJSON (User name age) =
    object [ "name" .= name
           , "age"  .= age
           ]
