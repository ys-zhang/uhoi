{-# LANGUAGE DefaultSignatures #-}

module Data.Type.Show where

import Data.Proxy
import GHC.TypeLits

class TShow (a :: k) where
  type ShowT a :: Symbol
  showT :: Proxy a -> String
  default showT :: KnownSymbol (ShowT a) => Proxy a -> String
  showT _ = symbolVal (Proxy @(ShowT a))

instance KnownSymbol s => TShow (s :: Symbol) where
  type ShowT s = s

instance KnownNat n => TShow (n :: Nat) where
  type ShowT n = NatVal n
  showT _ = show (natVal (Proxy @n))

type family NatVal (n :: Nat) :: Symbol where
  NatVal 0 = "0"
  NatVal 1 = "1"
  NatVal 2 = "2"
  NatVal 3 = "3"
  NatVal 4 = "4"
  NatVal 5 = "5"
  NatVal 6 = "6"
  NatVal 7 = "7"
  NatVal 8 = "8"
  NatVal 9 = "9"
  NatVal 10 = "10"
  NatVal 11 = "11"
  NatVal 12 = "12"
  NatVal 13 = "13"
  NatVal 14 = "14"
  NatVal 15 = "15"
  NatVal 16 = "16"
  NatVal 17 = "17"
  NatVal 18 = "18"
  NatVal 19 = "19"
  NatVal 20 = "20"
  NatVal n = TypeError ('Text "NatVal only supports up to 20, got: " ':<>: ShowType n)