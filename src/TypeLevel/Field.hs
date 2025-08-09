{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel.Field (
  Field(..), Div, NormSort
) where

import Data.Type.Ord (Compare, OrdCond)
import Data.Type.Equality
import Data.Type.Bool (If)
import GHC.TypeLits (Nat, type (+), type (-))
import TypeLevel.Utils (type (++))
import TypeLevel.Utils qualified as Utils

-- Expression type for type-level computations
data Field a 
  = One
  | Lit a 
  | Mul (Field a) (Field a) 
  | Inv (Field a)
  deriving (Eq, Show, Ord)
type Div a b = Mul a (Inv b)

type instance Compare (Lit a) (Lit b) = Compare a b
type instance Compare One (Lit a) = 'LT
type instance Compare (Lit a) One = 'GT

type family Norm (e :: Field a) :: ([Field a], [Field a]) where 
  Norm One         = '( '[], '[] )
  Norm (Lit a)     = '( '[Lit a], '[] )
  Norm (Inv One)   = Norm One
  Norm (Inv e)     = Utils.Swap (Norm e)
  Norm (Mul One e) = Norm e
  Norm (Mul e One) = Norm e
  Norm (Mul e1 e2) = Utils.ZipTuple Utils.OpListAppend (Norm e1) (Norm e2)

type family NormSort (e :: Field a) :: ([(Nat, Field a)], [(Nat, Field a)]) where
  NormSort e = Normalise 
                '( '[], '[]) 
                 (GroupTuple (ProdOneTuple (Utils.MapTuple Utils.OpSort (Norm e))))

type family ProdOneList x where
  ProdOneList '[] = '[]
  ProdOneList (x ': xs) = '(1, x) ': ProdOneList xs
type family ProdOneTuple x where
  ProdOneTuple '(x, y) = '(ProdOneList x, ProdOneList y)

type family GroupTuple pair where
  GroupTuple '(x, y) = '(Group x, Group y)
type family Group (xs :: [(Nat, Field a)]) :: [(Nat, Field a)] where
  Group '[] = '[]
  Group '[x] = '[x]
  Group ('(n, x) ': '(m, y) ': ps) = If (x == y) 
                                        (Group ('(n + m, x) ': ps)) 
                                        ('(n, x) ': Group ('(m, y) ': ps))

type family Normalise rs x where
  Normalise '(rxs, rys) '( '[], ys ) = '(Utils.ReverseList rxs, Utils.ReverseList rys ++ ys)
  Normalise '(rxs, rys) '( xs, '[] ) = '(Utils.ReverseList rxs ++ xs, Utils.ReverseList rys)
  Normalise '(rxs, rys) '( '(n, x) ': xs ,  '(m, y) ': ys ) = 
    OrdCond (Compare x y)
       ( Normalise '( '(n, x) ': rxs , rys          ) 
                   '( xs             , '(m, y) ': ys)
       )
       ( OrdCond (Compare n m)
            ( Normalise '(rxs, '(m-n, y) ': rys)  -- case LT
                        '(xs, ys)
            )
            ( Normalise '(rxs, rys) '(xs, ys) )   -- case EQ
            ( Normalise '( '(n-m, x) ': rxs, rys) 
                        '(xs, ys)
            )
       )
       ( Normalise '( rxs           , '(m, y) ': rys) 
                   '( '(n, x) ': xs ,  ys)
       )
