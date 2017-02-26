{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Json
  ( JsonApi
  , jsonApi
  , jsonServer
  ) where

import           Data.Proxy
import           Data.Text

import           Database.Persist
import           Database.Persist.Sql

import           Servant

import           Control.Monad.IO.Class
import           Models

import           Servant.API

type JsonApi =
  "api" :>
    (
      "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
      :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
      :<|> "users" :> Get  '[JSON] [User]
    )

jsonApi :: Proxy JsonApi
jsonApi = Proxy

jsonServer :: ConnectionPool -> Server JsonApi
jsonServer pool =
  userAddH
  :<|> userGetH
  :<|> usersGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name
    usersGetH = liftIO $ usersGet

    userAdd :: User -> IO (Maybe (Key User))
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UserName ==. (userName newUser)] []
      case exists of
        Nothing -> Just <$> insert newUser
        Just _ -> return Nothing

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

    usersGet :: IO [User]
    usersGet = flip runSqlPersistMPool pool $ do
      mUsers <- selectList [] []
      return $ fmap (\mUser -> entityVal mUser) mUsers
