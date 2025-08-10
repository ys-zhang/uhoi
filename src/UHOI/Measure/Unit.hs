{-# LANGUAGE UndecidableInstances #-}

module UHOI.Measure.Unit (Unit(..)) where

import Data.Proxy
import Data.Type.Show
import GHC.TypeLits (AppendSymbol)

data Unit  
  = Liter 
  | Gram   
  | Mol    
  | Newton 
  | (:/) Unit Unit
  | (:*) Unit Unit
  | Milli Unit 
  | Kilo  Unit 

infix 8 :/ 
infix 8 :*

instance TShow Liter where
  type ShowT Liter = "L"

instance TShow Gram where
  type ShowT Gram = "g"

instance TShow Mol where
  type ShowT Mol = "mol"

instance TShow Newton where
  type ShowT Newton = "N"

instance (TShow a, TShow b) => TShow (a :/ b) where
  type ShowT (a :/ b) = ShowT a `AppendSymbol` "/" `AppendSymbol` ShowT b
  showT _ = showT (Proxy @a) ++ "/" ++ showT (Proxy @b)

instance (TShow a, TShow b) => TShow (a :* b) where
  type ShowT (a :* b) = ShowT a `AppendSymbol` "*" `AppendSymbol` ShowT b
  showT _ = showT (Proxy @a) ++ "*" ++ showT (Proxy @b)

-- TODO: currently for demonstration purpose only,
-- the `TShow` instance for `Kilo` and `Milli` is incorrect

instance TShow a => TShow (Milli a) where
  type ShowT (Milli a) = "m" `AppendSymbol` ShowT a
  showT _ = "m" ++ showT (Proxy @a)

instance TShow a => TShow (Kilo a) where
  type ShowT (Kilo a) = "k" `AppendSymbol` ShowT a
  showT _ = "k" ++ showT (Proxy @a)



