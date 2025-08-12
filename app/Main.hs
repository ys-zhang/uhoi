{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.WebServer qualified as WS
import Control.Monad (when, forM_)
import Data.SOP.Table
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Lucid
import Options.Applicative
import System.Directory
import System.FilePath ( (</>), (<.>) )
import UHOI (conceptsMetaTable)


data Options 
  = WebServerOptions WS.Options
  | CmdOptions CmdOptions

data CmdOptions 
  = PrettyPrint
  | GenHtml GenHtmlOptions

newtype GenHtmlOptions = GenHtmlOptions
  { outputPath :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser = WebServerOptions <$> parse_web_options
              <|> CmdOptions <$> parse_cmd_options
 where
  parse_web_options = hsubparser
    ( command "run-server" 
        (info WS.optionsParser  
              (progDesc "Run the web server (options: --port, --host)")
        )
    )
  parse_cmd_options = hsubparser
    (  command "gen-html" 
         (info (GenHtml <$> parseGenHtmlOptions) 
               (progDesc "Generate HTML (options: -o/--output)")
         )
    <> command "pretty-print" 
         (info (pure PrettyPrint <**> helper) 
               (progDesc "Pretty print the concept meta table")
         )
    )
  parseGenHtmlOptions = GenHtmlOptions
    <$> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Output file for generated HTML" ))


main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) (info (optionsParser <**> helper)
             ( fullDesc
            <> progDesc "UHOI command line interface"
            <> header "uhoi - a Haskell application" ))
  case opts of
    WebServerOptions wsOpts -> WS.app wsOpts
    CmdOptions cmdOpts -> runCmd cmdOpts
 where
  runCmd PrettyPrint = do
    putStrLn "\nConcept Meta Table:"
    putStrLn $ prettyHTable conceptsMetaTable
  runCmd (GenHtml opts) = do
    case opts.outputPath of 
      Nothing -> do 
        putStrLn 
          $ prettyPrintHtml 
          $ WS.getStaticPage' "meta-table"
      Just path -> do
        putStrLn $ "Writing HTML to " ++ path
        output_exists <- doesDirectoryExist path
        when output_exists do
          putStrLn $ "Warning: Output directory already exists, remove the path: " 
                   ++ path
          removeDirectoryRecursive path
        createDirectoryIfMissing True path
        forM_ (Map.toList WS.staticPages) \(route, html) -> do
          writeFile (path </> route <.> "html") 
                    (prettyPrintHtml html)

prettyPrintHtml :: Html () -> String
prettyPrintHtml html = raw_str
 where
  raw_str = TL.unpack $ renderText html