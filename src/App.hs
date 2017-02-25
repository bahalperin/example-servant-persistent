{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import           Data.String.Conversions
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy

import qualified Network.HTTP.Types         as HTTP
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant
import           Servant.Docs
import           Servant.Utils.StaticFiles

import           Data.Text
import           Text.Blaze

import           Api
import           Api.Json (jsonApi)
import           Models


server :: ConnectionPool -> Server Api
server pool =
  (userAddH :<|> userGetH :<|> usersGetH)
  :<|> (fmap unsafeByteString (liftIO $ BS.readFile "static/index.html") :<|> serveDirectory "static")
  :<|> serveDocs
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

    serveDocs :: Application
    serveDocs _ respond =
        respond $ responseLBS HTTP.ok200 [plain] docsBS

    plain = ("Content-Type", "text/plain")

docsBS :: BSL.ByteString
docsBS = encodeUtf8
       . Data.Text.Lazy.pack
       . markdown
       $ docsWithIntros [intro] jsonApi
  where intro = DocIntro "Servant Example App" ["This is the documentation for the Servant Example App API.", "Enjoy!"]

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
