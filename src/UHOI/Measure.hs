{-# LANGUAGE UndecidableInstances #-}
module UHOI.Measure (
  module UHOI.Measure.Unit,
  MeasureUnit,
  MMOL_PER_L,
)  where

import Data.Proxy
import Data.String (IsString(..))
import Data.Type.Show
import Interpreter.UI
import UHOI.Measure.Unit
import Meta


class MeasureUnit (u :: Unit) where

instance Feature MeasureUnit where
  data (:=) MeasureUnit unit

instance TShow MeasureUnit where
  type ShowT MeasureUnit = "MeasureUnit"

instance MeasureUnit (u :: Unit) 

instance {-# OVERLAPPABLE #-} 
   ( MeasureUnit u
   , TShow u
   ) 
  => HasCell (MeasureUnit := u) where
  type ColT (MeasureUnit := u) = AsType MeasureUnit
  cell _ = fromString $ showT (Proxy @u)

type MMOL_PER_L = Milli Mol :/ Liter 


-- >>> showT (Proxy @MMOL_PER_L)
-- "mmol/L"
