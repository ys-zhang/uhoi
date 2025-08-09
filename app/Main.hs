module Main where

import Data.SOP
import Data.SOP.Table
import Data.Text (Text, pack)
import qualified Data.Text.Lazy.IO as TL
import UHOI (Concepts, conceptsMetaTable)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "TUI Table:"
  putStrLn s
  putStrLn "\nHTML Table:"
  TL.putStrLn $ toHtmlTable textTable
  putStrLn "\nConcept Meta Table:"
  putStrLn (prettyHTable conceptsMetaTable)

table :: UTable String ["Name", "Age"]
table = MkHTable [ K "John" :* K "25" :* Nil
                 , K "Jane" :* K "30" :* Nil 
                 ]

-- Convert to Text table for HTML rendering
textTable :: UTable Text ["Name", "Age"]
textTable = MkHTable [ K (pack "John") :* K (pack "25") :* Nil
                     , K (pack "Jane") :* K (pack "30") :* Nil 
                     ]

s :: String
s = prettyHTable table

-- mtable :: UI.MetaTable '[ConceptExample]
-- mtable = UI.table (Proxy @'[ConceptExample]) (hpure Proxy)

