{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}

module Interpreter.UI (
  -- * Table
  --
  -- $note-impl-table 

  -- ** Column and Cell
  Cell(..),
  HasCol(..), missingValues,  
  HasCell(..), 
  -- ** Row 
  HasRow(..), 
  -- ** Table
  MetaTable,
  HasTable(..), 

  -- * reexport
  module Data.Type.AsType,
) where

import Data.Default (Default(..))
import Data.Kind
import Data.Singletons
import Data.SOP
import Data.SOP.Table (HTable(..))
import Data.SOP.Table qualified as Tbl
import Data.SOP.Utils (Subset(..), SetUnion(..),)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.AsType
import Data.Type.Show
import GHC.TypeLits
import Meta

-- ===========================================================================
-- Table
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- $note-impl-table
--
-- Default intances: 
--   + there is a default `HasCol` instance for all `Feature` instances
--   + there is a default `HasCol` instance for `feat := a` if `feat |- a` holds
--   + there is a default `HasCol` instance for `a :> b` if both `a` and `b` are `HasCol`
--
-- Implementation Steps
--   + implement `HasCell` for type `feat := a` given `feat |- a`

{-
   feat := a  <------ HasCell: ColT     = feat
   feat       <------ HasCol:  MetaColT = MetaCol feat
                               CellT    = Text
   
-}

-- ===========================================================================

newtype Cell a = MkCell { value :: CellT a }

deriving stock instance Show (CellT a) => Show (Cell a)
deriving newtype instance Eq (CellT a) => Eq (Cell a)
deriving newtype instance Ord (CellT a) => Ord (Cell a)
deriving newtype instance IsString (CellT a) => IsString (Cell a) 

instance {-# OVERLAPS #-} CellT (Name n) ~ Text => Tbl.ShowCell plt (Cell (Name (n :: Symbol))) where
  showCell _ = T.unpack . (.value)

instance {-# OVERLAPS #-} CellT (AsType a) ~ Text => Tbl.ShowCell plt (Cell (AsType (a :: k -> Constraint))) where
  showCell _ = T.unpack . (.value)

instance {-# OVERLAPS #-} Show (CellT a) => Tbl.ShowCell plt (Cell a) where
  showCell _ = show . (.value)

class HasCol (a :: k) where
  type CellT a :: Type
  type CellT a = Text
  colName :: Proxy a -> String
  cellMissingValue :: Proxy a -> CellT a
  default cellMissingValue :: Default (CellT a) => Proxy a -> CellT a
  cellMissingValue _ = def

instance {-# OVERLAPPABLE #-} KnownSymbol n => HasCol (Name (n :: Symbol)) where
  colName _ = symbolVal (Proxy @n)
  cellMissingValue _ = "/" 

instance {-# OVERLAPPABLE #-} Feature feat => HasCol (AsType (feat :: k -> Constraint))  where
  colName = showT
  cellMissingValue _ = "/"

-- instance {-# OVERLAPPABLE #-} feat |- a => HasCol (feat := a) where
--     type CellT (feat := a) = CellT feat
--     type MetaColT (feat := a) = MetaColT feat

instance (HasCol a , HasCol b) 
  => HasCol (a :> b) where
    type CellT (a :> b) = CellT b
    colName _ = colName (Proxy @a) ++ colName (Proxy @b)
    cellMissingValue _ = cellMissingValue (Proxy @b)


class HasCol (ColT a) => HasCell a where
  type ColT a :: Type 
  cell :: Proxy a -> CellT (ColT a)


-- | HasRow: each concept will implement the HasRow instance
class HasRow (a :: k) where
  type RowT a :: [Type]
  row :: Proxy a -> NP Cell (RowT a)

instance {-# OVERLAPPABLE #-} (HasRow fs, KnownSymbol n)
  => HasRow (Concept n fs) where
    type RowT (Concept n fs) = ConceptName ': RowT fs
    row :: Proxy (Concept n fs) -> NP Cell (RowT (Concept n fs))
    row _ =
      let name = MkCell @ConceptName . T.pack $ symbolVal (Proxy @n)
          rs = row (Proxy @fs) :: NP Cell (RowT fs :: [Type])
      in  name :* rs

instance HasRow '[] where
  type RowT '[] = '[]
  row _ = Nil

instance {-# OVERLAPPABLE #-} 
  --  ( All HasCol (MapMetaColT fs)
  --  , All HasCell fs
  --  , MapMetaColT fs ~ TU.Unique (MapMetaColT fs)
  --  ) 
    ( HasRow fs
    , HasCell f 
    , HasCol (ColT f)
    )
  => HasRow (f ': fs :: [Type]) where
    type RowT (f ': fs) = ColT f ': RowT fs
    row :: Proxy (f ': fs) -> NP Cell (RowT (f ': fs))
    row _ = MkCell (cell $ Proxy @f) :* row (Proxy @fs)

type MetaTable cs = HTable Cell (TableT cs)

-- | HasTable: concept collection shall implement the HasTable instance
class HasTable (a :: k) where
  type TableT a :: [Type]
  table :: TableT a `Subset` full 
        => Proxy a 
        -> NP Cell full 
        -> HTable Cell full

instance HasTable '[] where
  type TableT '[] = '[]
  table _ _ = mempty

instance {-# OVERLAPPABLE #-} 
  ( HasRow a
  , HasTable as
  , SetUnion (RowT a) (TableT as) 
  , TableT as `Subset` SetUnionT (RowT a) (TableT as)
  ) 
  => HasTable (a ': as) where 
    type TableT (a ': as) = SetUnionT (RowT a) (TableT as)
    table :: SetUnionT (RowT a) (TableT as) `Subset` full 
          => Proxy (a ': as) 
          -> NP Cell full 
          -> HTable Cell full
    table _ full = 
      rest_tbl 
        { rows = head_row 
               : map (`bulkUpdateNP` full) 
                     rest_tbl.rows 
        }
     where
      uion_row = unionBulkUpdate @_ @_ @(TableT as) 
                   (Left (row (Proxy @a)))
                   exact
      exact    = subset @_ @(SetUnionT (RowT a) (TableT as)) full 
      head_row = bulkUpdateNP uion_row full
      rest_tbl = table (Proxy @as) exact

missingValues :: forall as.  All HasCol as => Proxy as -> NP Cell as
missingValues _ = 
  hcmap (Proxy @HasCol) (MkCell . cellMissingValue) ps
 where
  ps :: NP Proxy as
  ps = hpure Proxy

-- >>> :t row
-- row :: forall k (a :: k). HasRow a => Proxy a -> NP Cell (RowT a)
