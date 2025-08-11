{-# LANGUAGE OverloadedStrings #-}

module App.WebServer (
  Options, optionsParser,
  app
) where

import Network.Wai
import Network.Wai.Handler.Warp
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import UHOI (conceptsMetaTable)
import Data.SOP.Table (renderHtmlTable)
import Options.Applicative

data Options = Options

optionsParser :: Parser Options
optionsParser = pure Options

type Api = Get '[HTML] (Html ())
      :<|> "test" :> Get '[HTML] (Html ())

api :: Proxy Api 
api = Proxy

app :: Options -> IO ()
app _ = do 
  putStrLn "Starting web server on port 8080..."
  run 8080 . logRequests $ serve api handler

logRequests :: Middleware
logRequests waiApp req resp = do
  putStrLn $ "Request method: " ++ show (requestMethod req)
  putStrLn $ "Request path: " ++ show (rawPathInfo req)
  waiApp req resp

handler :: Server Api
handler = pure meta_table :<|> pure hello_world
 where 
  hello_world = html_ $ do 
    h1_ "Hello, World!"
    p_ $ do 
      "Jump to the " <> a_ [href_ "/meta"] "meta table"
  meta_table = html_ $ do
    head_ $ title_ "Concepts Meta Table"
    body_ conceptsHtmlTable

conceptsHtmlTable :: Html ()
conceptsHtmlTable = renderHtmlTable [] conceptsMetaTable
