{-# LANGUAGE UndecidableInstances #-}
module Data.SOP.Table where

import Data.Kind (Type)
import Data.SOP
import Data.SOP.Constraint (SListIN)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.AsType
import Data.Type.Show
import Lucid
import Text.Layout.Table

type data TUI
type data LUCID

class Platform plt where
  type PlatformData plt :: Type

instance Platform TUI where
  type PlatformData TUI = String

instance Platform LUCID where
  type PlatformData LUCID = Html ()


class ShowCell plt a where
  showCell :: Proxy plt -> a -> PlatformData plt

instance {-# OVERLAPS #-} IsString (PlatformData plt) => ShowCell plt (K String a) where
  showCell _ = fromString . unK

instance {-# OVERLAPS #-} IsString (PlatformData plt) => ShowCell plt (K Text a) where
  showCell _ = fromString . T.unpack . unK

instance {-# OVERLAPS #-} (Show a, IsString (PlatformData plt)) => ShowCell plt (K a x) where
  showCell _ = fromString . show . unK

instance {-# OVERLAPPABLE #-} (Show a, IsString (PlatformData plt) ) => ShowCell plt a where
  showCell _ = fromString . show 

showTuiCell :: ShowCell TUI a => a -> String
showTuiCell = showCell (Proxy @TUI)

renderLucidCell :: ShowCell LUCID a => a -> Html ()
renderLucidCell = showCell (Proxy @LUCID)

-- =====================================================================
-- Table Definition
-- =====================================================================

class Column (a :: k) where
  columnName :: IsString str => Proxy a -> str
  ppColumnSpec :: Proxy a -> ColSpec
  ppColumnSpec _ = defColSpec

instance {-# OVERLAPS #-} TShow a => Column (AsType a) where
  columnName _ = fromString (showT (Proxy @a))

instance {-# OVERLAPPING #-} TShow a => Column a where
  columnName _ = fromString (showT (Proxy @a))

type HTable :: (k -> Type) -> [k] -> Type
newtype HTable cellF cols = MkHTable
  { rows :: [NP cellF cols]
  }
  deriving newtype (Semigroup, Monoid)

deriving instance Show (NP cellF cols) => Show (HTable cellF cols) 

type instance SListIN HTable = SListI
type instance AllN HTable c = All c
type instance Prod HTable = NP

mapHRows :: (NP cellF xs -> NP cellF ys) -> HTable cellF xs -> HTable cellF ys
mapHRows f (MkHTable rows) = MkHTable (map f rows)

-- | create a single row table
instance HPure HTable where
  hpure a = MkHTable [hpure a]
  hcpure c x = MkHTable [hcpure c x]

-- | apply a heterogeneous list of columns function to each row
instance HAp HTable where
  hap col_fs tbl = MkHTable $ map (hap col_fs) tbl.rows

-- | a homogeneous table with a fixed cell type
type UTable cell cols = HTable (K cell) cols

-- | cast a heterogeneous table to a homogeneous table for render in tui
toTuiUTable
  :: forall cellF cols . All (Compose (ShowCell TUI) cellF) cols
  => HTable cellF cols -> UTable String cols
toTuiUTable = hcmap (Proxy @(Compose (ShowCell TUI) cellF)) (K . showTuiCell)

-- | cast a heterogeneous table to a homogeneous table for render in html
toLucidUTable 
  :: forall cellF cols . All (Compose (ShowCell LUCID) cellF) cols
  => HTable cellF cols -> UTable (Html ()) cols
toLucidUTable = hcmap (Proxy @(Compose (ShowCell LUCID) cellF)) (K . renderLucidCell)

-- =====================================================================
-- TUI
-- =====================================================================

-- | Pretty print a UTable with String cells using table-layout
prettyHTable
  :: forall cellF cols
  .  ( All Top cols
     , All Column cols
     , All (Compose (ShowCell TUI) cellF) cols
     )
  => HTable cellF cols -> String
prettyHTable =
  prettyUTableWith colSpec headerSpec . toTuiUTable
 where
  headers = hcmap (Proxy :: Proxy Column) (K . columnName) cols
  colSpec = hcmap (Proxy :: Proxy Column) (K . ppColumnSpec) cols
  headerSpec = titlesH (hcollapse headers)
  cols = hpure Proxy

-- | Pretty print with custom column specifications
prettyUTableWith :: All Top cols
                 => NP (K ColSpec) cols
                 -> HeaderSpec LineStyle String
                 -> UTable String cols
                 -> String
prettyUTableWith specs headerSpec tbl = tableString tableSpec
 where
  tableSpec = columnHeaderTableS colSpecsList unicodeS headerSpec (map rowG rowData)
  rowData = map hcollapse tbl.rows
  colSpecsList = hcollapse specs


-- =====================================================================
-- Html
-- =====================================================================

-- | Render a UTable with Text cells to HTML using Lucid
renderHtmlTable
  :: forall cellF cols
  .  ( All Top cols
     , All Column cols
     , All (Compose (ShowCell LUCID) cellF) cols
     )
  => [Attributes]
  -> HTable cellF cols
  -> Html ()
renderHtmlTable as = renderHtmlUTable as . toLucidUTable

-- | Render with custom CSS classes
renderHtmlUTable
  :: forall cols
  .  (All Top cols, All Column cols)
  => [Attributes]  -- ^ Table attributes (e.g., CSS classes)
  -> UTable (Html ()) cols
  -> Html ()
renderHtmlUTable attrs tbl = 
  table_ attrs $ do
    thead_ $ tr_ $ mconcat headerCells
    tbody_ $ mapM_ renderRow tbl.rows
 where
  cols = hpure Proxy :: NP Proxy cols
  headerCells :: [Html ()]
  headerCells = hcollapse $ hcmap (Proxy :: Proxy Column) (K . th_ . toHtml @String . columnName) cols
  renderRow :: NP (K (Html ())) cols -> Html ()
  renderRow row = tr_ . mconcat $ (hcollapse $ hmap (K . td_ . unK) row :: [Html ()])


