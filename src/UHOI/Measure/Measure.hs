module Measure (Unit(..)) where

import TypeLevel.Field
import GHC.TypeLits (Symbol)

data Unit (a :: Field Symbol) where
  Liter  :: Unit (Lit "Volumn")
  Gram   :: Unit (Lit "Mass")
  Mol    :: Unit One
  Newton :: Unit (Lit "Force")
  (:/)   :: Unit a -> Unit b -> Unit (a `Div` b)
  (:*)   :: Unit a -> Unit b -> Unit (a `Mul` b)
  Milli  :: Unit a -> Unit a
  Kilo   :: Unit a -> Unit a

infix 8 :/ 
infix 8 :*
