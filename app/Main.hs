module Main where

import Data.SOP.Table
import Data.Text.Lazy qualified as TL
import Lucid
import UHOI (conceptsMetaTable)
import Options.Applicative
import App.WebServer qualified as WS

newtype Options = Options
  { runServer :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
        ( long "run-server"
       <> help "Run the web server" )

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Run the application"
   <> header "uhoi - a Haskell application" )
  if runServer opts
    then WS.app
    else do
      putStrLn "\nConcept Meta Table:"
      putStrLn $ prettyHTable conceptsMetaTable
      putStrLn "\nConcept Meta Table (HTML):"
      putStrLn $ prettyPrintHtml conceptsHtmlTable

conceptsHtmlTable :: Html ()
conceptsHtmlTable = renderHtmlTable [] conceptsMetaTable


-- mtable :: UI.MetaTable '[ConceptExample]
-- mtable = UI.table (Proxy @'[ConceptExample]) (hpure Proxy)

prettyPrintHtml :: Html () -> String
prettyPrintHtml html = raw_str
 where
  raw_str = TL.unpack $ renderText html