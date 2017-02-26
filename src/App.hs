{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger      (runStderrLoggingT)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.String.Conversions
import qualified Data.Text.Lazy
import           Data.Text.Lazy.Encoding   (encodeUtf8)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import qualified Network.HTTP.Types        as HTTP

import           Network.Wai
import           Network.Wai.Handler.Warp  as Warp

import           Servant
import           Servant.Docs
import           Servant.Utils.StaticFiles

import           Data.Text
import           Text.Blaze

import           Api                       (Api, api)
import           Api.Docs                  (serveDocs)
import           Api.Json                  (jsonServer)
import           Api.StaticFiles           (staticFilesServer)
import           Models


server :: ConnectionPool -> Server Api
server pool =
  jsonServer pool
  :<|> staticFilesServer
  :<|> serveDocs

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile = do
  putStrLn "Server running on port 3000"
  Warp.run 3000 =<< mkApp sqliteFile
