{-# LANGUAGE UndecidableInstances #-}
module Meta where

import Data.Kind
import Data.Proxy
import Data.Type.Show
import GHC.TypeLits

data (path :: k1) :> (a :: k2)
data Doc (str :: Symbol) (a :: k)

data Concept (name :: Symbol) (fs :: [Type])

class TShow feat => Feature (feat :: k -> Constraint) where
  data (:=) feat (a :: k)
  infixr 1 :=

type family FeatKey pair where
  FeatKey (feat := a) = feat

type family FeatVal pair where
  FeatVal (feat := a) = a

featKey :: feat a => Proxy (feat := a) -> Proxy feat
featKey _ = Proxy

featVal :: feat a => Proxy (feat := a) -> Proxy a
featVal _ = Proxy

type (|-) :: (k -> Constraint) -> k -> Constraint
type (|-) (a :: k -> Constraint) (b :: k) = (Feature a, a b, TShow a)

type data Name (a :: k)

type ConceptName = Name "Concept"

instance {-# OVERLAPPING #-} TShow a => TShow (Name a) where
  type ShowT (Name a) = ShowT a `AppendSymbol` " Name"
  showT _ = showT (Proxy @a) <> " Name"

