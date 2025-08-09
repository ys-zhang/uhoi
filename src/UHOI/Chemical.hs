{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module UHOI.Chemical where

import Data.Kind
import Data.Proxy
import Data.String (IsString(..))
import Data.Type.Show
import GHC.TypeLits
import Meta
import Interpreter.UI

class Chemical (a :: Type)  where
  type ChemicalName a :: Symbol 
  type ChemicalName a = ShowT a
  chemicalName :: Proxy a -> String
  default chemicalName :: TShow a => Proxy a -> String
  chemicalName = showT

instance TShow Chemical where
  type ShowT Chemical = "Chemical"

instance Feature Chemical where
  data (:=) Chemical chemical

chemical :: Chemical c => Proxy (Chemical := c) -> Proxy c
chemical = featVal


instance {-# OVERLAPPABLE #-} Chemical c => HasCell (Chemical := c) where
  type ColT (Chemical := c) = AsType Chemical
  cell _ = fromString $ chemicalName (Proxy @c)

-- >>> :kind! CellT Chemical
-- CellT Chemical :: *
-- = Text

-- =====================================================================
-- Protein
-- =====================================================================

data Protein (name :: Symbol) (variant :: Nat)
  deriving anyclass Chemical

instance (KnownSymbol name, KnownNat variant) => TShow (Protein name variant) where
  type ShowT (Protein name variant) = 
    "Protein " `AppendSymbol` name `AppendSymbol` " " `AppendSymbol` ShowT variant
  showT _ = "Protein " ++ symbolVal (Proxy @name) ++ " " ++ show (natVal (Proxy @variant))