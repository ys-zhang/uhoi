module Data.Type.AsType where

import Data.Kind
import Data.Proxy
import Data.Type.Show

type family MapAsType (xs :: [k]) :: [Type] where
  MapAsType '[] = '[]
  MapAsType (x ': xs) = AsType x ': MapAsType xs

type data AsType (a :: k)

instance {-# OVERLAPPABLE #-} TShow tag => TShow (AsType tag) where
  type ShowT (AsType tag) = ShowT tag
  showT _ = showT (Proxy @tag)
