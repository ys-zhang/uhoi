{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module UHOI (
  module UHOI.Chemical,
  module UHOI.Measure,
  module Meta,
  mkMetaTable,
  -- * concepts
  Concepts,
  conceptsMetaTable,
  -- ** concept definitions
  ActinLike9,
  ActinLike10,

) where

import Data.SOP
import Data.SOP.Utils (Subset)
import Meta
import UHOI.Chemical
import UHOI.Measure
import Interpreter.UI


type Concepts = 
  '[ ActinLike9
   , ActinLike10
   , ActinLike11
   , MissingChemical  
   , MissingMeasureUnit
   ]

type ActinLike9
  = Concept "Actin-like 9" 
      '[ Chemical    := Protein "Actin-like" 9
       , MeasureUnit := Milli Mol :/ Liter
       ]

type ActinLike10
  = Concept "Actin-like 10" 
      '[ Chemical    := Protein "Actin-like" 10
       , MeasureUnit := Milli Mol :/ Kilo Liter
       ]

type ActinLike11
  = Concept "Actin-like 11"
      '[ Chemical    := Protein "Actin-like" 11
       , MeasureUnit := Milli Mol 
       ]

type MissingChemical
  = Concept "Missing Chemical"
      '[ MeasureUnit := Milli Mol :/ Liter
       ]

type MissingMeasureUnit
  = Concept "Missing MeasureUnit"
      '[ Chemical := Protein "Missing Chemical" 0
       ]

mkMetaTable :: forall concepts 
            . ( HasTable concepts
              , All HasCol (TableT concepts)
              , Subset (TableT concepts) (TableT concepts)
              )
            => Proxy concepts 
            -> MetaTable concepts
mkMetaTable p = table p missing
 where
  missing = missingValues (Proxy @(TableT concepts))

conceptsMetaTable :: MetaTable Concepts
conceptsMetaTable = mkMetaTable (Proxy @Concepts)




-- ==================================================================================================================
-- Internal Testing
-- ==================================================================================================================

-- >>> conceptsMetaTable
-- MkHTable {rows = [MkCell {value = "Actin-like 9"} :* MkCell {value = "Protein Actin-like 9"} :* MkCell {value = "mmol/L"} :* Nil,MkCell {value = "Actin-like 10"} :* MkCell {value = "Protein Actin-like 10"} :* MkCell {value = "mmol/kL"} :* Nil,MkCell {value = "Missing Chemical"} :* MkCell {value = "/"} :* MkCell {value = "mmol/L"} :* Nil,MkCell {value = "Missing MeasureUnit"} :* MkCell {value = "Protein Missing Chemical 0"} :* MkCell {value = "/"} :* Nil]}

-- >>> headers (Proxy @ConceptExample)
-- ["Concept Name","Chemical","MeasureUnit"]

-- >>> :kind! (TableT Concepts)
-- (TableT Concepts) :: [*]
-- = '[Name "Concept", AsType Chemical, AsType MeasureUnit]


-- >>> :kind! (ColT (Chemical := Protein "Actin-like" 9))
-- (ColT (Chemical := Protein "Actin-like" 9)) :: *
-- = AsType Chemical

-- >>> missingValues (Proxy @(TableT Concepts))
-- MkCell {value = "/"} :* MkCell {value = "/"} :* MkCell {value = "/"} :* Nil

-- >>> :kind! RowT ActinLike9
-- RowT ActinLike9 :: [*]
-- = '[Name "Concept", AsType Chemical, AsType MeasureUnit]

-- >>> row (Proxy @ActinLike9)
-- MkCell {value = "Actin-like 9"} :* MkCell {value = "Protein Actin-like 9"} :* MkCell {value = "mmol/L"} :* Nil

-- >>> row (Proxy @ActinLike10)
-- MkCell {value = "Actin-like 10"} :* MkCell {value = "Protein Actin-like 10"} :* MkCell {value = "mmol/kL"} :* Nil

-- a9 = row (Proxy @ActinLike9)

-- ms = missingValues (Proxy @(TableT Concepts))

-- x = bulkUpdateNP a9 ms


-- >>> x
-- MkCell {value = "Actin-like 9"} :* MkCell {value = "Protein Actin-like 9"} :* MkCell {value = "mmol/L"} :* Nil

-- >>> a9
-- MkCell {value = "Actin-like 9"} :* MkCell {value = "Protein Actin-like 9"} :* MkCell {value = "mmol/L"} :* Nil
