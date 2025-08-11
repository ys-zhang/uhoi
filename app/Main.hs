{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.SOP.Table
import Data.Text.Lazy qualified as TL
import Lucid
import UHOI (conceptsMetaTable)
import Options.Applicative
import App.WebServer qualified as WS

data Options 
  = WebServerOptions WS.Options
  | CmdOptions CmdOptions

data CmdOptions = GenHtml | PrettyPrint

optionsParser :: Parser Options
optionsParser = WebServerOptions <$> parse_web_options
              <|> CmdOptions <$> parse_cmd_options
 where
  parse_web_options = subparser
    ( command "run-server" (info WS.optionsParser (progDesc "Run the web server"))
    )
  parse_cmd_options = subparser
    ( command "gen-html" (info (pure GenHtml) (progDesc "Generate HTML"))
    <> command "pretty-print" (info (pure PrettyPrint) (progDesc "Pretty print"))
    )


main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Run the application"
   <> header "uhoi - a Haskell application" )
  case opts of
    WebServerOptions wsOpts -> WS.app wsOpts
    CmdOptions cmdOpts -> runCmd cmdOpts
 where
  runCmd PrettyPrint = do
    putStrLn "\nConcept Meta Table:"
    putStrLn $ prettyHTable conceptsMetaTable
  runCmd GenHtml = do
    putStrLn . prettyPrintHtml $ do 
      html_ $ do
        head_ $ title_ "Concepts Meta Table"
        body_ conceptsHtmlTable

conceptsHtmlTable :: Html ()
conceptsHtmlTable = renderHtmlTable [] conceptsMetaTable

-- mtable :: UI.MetaTable '[ConceptExample]
-- mtable = UI.table (Proxy @'[ConceptExample]) (hpure Proxy)

prettyPrintHtml :: Html () -> String
prettyPrintHtml html = raw_str
 where
  raw_str = TL.unpack $ renderText html