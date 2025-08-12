{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module App.WebServer (
  Options, optionsParser,
  app,
  -- * static pages 
  staticPages, 
  getStaticPage, 
  getStaticPage',
) where

import Control.Monad (void)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (fromString)
import Data.SOP.Table (renderHtmlTable)
import Network.Wai
import Network.Wai.Handler.Warp
import Lucid
import Options.Applicative
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import UHOI (conceptsMetaTable)
import GHC.Stack (HasCallStack)

data Options = Options
  { port :: Int
  , host :: String
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8080
     <> help "Port number for the web server" )
  <*> strOption
      ( long "host"
     <> metavar "HOST"
     <> value "localhost"
     <> help "Host address for the web server" )

type Api = Get '[HTML] (Html ())
      :<|> "test" :> Get '[HTML] (Html ())

api :: Proxy Api 
api = Proxy

app :: Options -> IO ()
app opts = do 
  putStrLn $ "Starting web server on " ++ opts.host ++ ":" ++ show opts.port ++ "..."
  let settings = defaultSettings
               & setHost (fromString opts.host) 
               & setPort opts.port 
                 
  runSettings settings . logRequests $ serve api handler

logRequests :: Middleware
logRequests waiApp req resp = do
  putStrLn $ "Request method: " ++ show (requestMethod req)
  putStrLn $ "Request path: " ++ show (rawPathInfo req)
  waiApp req resp

handler :: Server Api
handler = pure (getStaticPage' "meta-table") :<|> pure hello_world
 where 
  hello_world = html_ $ do 
    h1_ "Hello, World!"
    p_ $ do 
      "Jump to the " <> a_ [href_ "./meta"] "meta table"

staticPages :: Map FilePath (Html ())
staticPages = 
  [ ( "index"
    , html_ do 
        void . head_ $ title_ "Type Level UHOI"
        body_ $ do
          h1_ "Type Level UHOI"
          ol_ $ do
            li_ $ a_ [href_ "./meta-table"] "Concepts Meta Table"
            li_ $ a_ [href_ "./haddocks"] "Documents"
    ) 
  , ( "meta-table"
    , html_ $ do   
        void . head_ $ title_ "Concepts Meta Table"
        body_ conceptsHtmlTable
    )
  ]

getStaticPage' :: HasCallStack => FilePath -> Html ()
getStaticPage' = fromJust . getStaticPage

getStaticPage :: FilePath -> Maybe (Html ())
getStaticPage = flip Map.lookup staticPages

conceptsHtmlTable :: Html ()
conceptsHtmlTable = renderHtmlTable [] conceptsMetaTable

-- >>> Map.keys staticPages
-- ["index","meta-table"]


