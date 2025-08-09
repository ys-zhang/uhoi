{-# LANGUAGE UndecidableInstances #-}

module Data.SOP.Utils (
  ElemOf(..), 
  Subset(..),  
  SetRemove(..),
  SetUnion(..),
) where

import Data.SOP
import TypeLevel.Utils qualified as TU

class ElemOf (xs :: [k]) (x :: k) where
  -- | update the first occurrence of an element in a type-level list
  -- >>> updateNP (K 0 :: K Int String) (K 1 :* K 2 :* Nil :: NP (K Int) '[String, Int] )
  -- K 0 :* K 2 :* Nil
  -- >>> updateNP (K 0 :: K Int Int) (K 1 :* K 2 :* Nil :: NP (K Int) '[String, Int] )
  -- K 1 :* K 0 :* Nil
  updateNP :: f x -> NP f xs -> NP f xs
  at :: Proxy x -> NP f xs -> f x

instance ElemOf (x ': xs) x where
  updateNP x (_ :* xs) = x :* xs
  at _ (x :* _) = x

instance {-# OVERLAPPABLE #-} ElemOf ys x => ElemOf (y ': ys) x where
  updateNP x (y :* ys) = y :* updateNP x ys
  at _ (_ :* ys) = at (Proxy @x) ys


class Subset (xs :: [k]) (ys :: [k]) where
  bulkUpdateNP :: NP f xs -> NP f ys -> NP f ys
  subset :: NP f ys -> NP f xs

instance {-# OVERLAPPING #-} Subset (x ':xs) ( x ': xs) where
  bulkUpdateNP ys _ = ys
  subset ys = ys

instance {-# OVERLAPPABLE #-} Subset '[] ys where
  bulkUpdateNP _ ys = ys
  subset _ = Nil

instance {-# OVERLAPPABLE #-} (ElemOf ys x, Subset xs ys) => Subset (x ': xs) ys where
  bulkUpdateNP (x :* xs) ys = updateNP x (bulkUpdateNP xs ys)
  subset ys = at (Proxy @x) ys :* subset ys

class SetRemove (a :: k) (as :: [k]) where
  -- | first remove the element from the NP then use the data update a full NP
  putback :: f a -> NP f (TU.Remove a as) -> NP f as
  remove :: Proxy a -> NP f as -> NP f (TU.Remove a as)

instance SetRemove a '[] where
  putback _ _ = Nil
  remove _ _ = Nil

instance (as ~ TU.Remove a (a ': as)) => SetRemove (a :: k) (a ': as :: [k]) where
  putback x xs = x :* xs
  remove _ (_ :* xs) = xs

instance {-# OVERLAPPABLE #-} 
  ( (b ': TU.Remove a as) ~ TU.Remove a (b ': as)
  , SetRemove a as )
  => SetRemove a (b ': as) where
    putback x (y :* ys) = y :* putback x ys
    remove x (y :* xs) = y :* remove x xs

class SetUnion (as :: [k]) (bs :: [k]) where
  type SetUnionT as bs :: [k]
  unionBulkUpdate 
    :: Either (NP f as) (NP f bs)
    -> NP f (SetUnionT as bs) 
    -> NP f (SetUnionT as bs) 

instance SetUnion '[] bs where
  type SetUnionT '[] bs = bs
  unionBulkUpdate (Right ys) _  = ys
  unionBulkUpdate (Left _) full = full

instance (SetUnion as bs, SetRemove a (SetUnionT as bs)) => SetUnion (a ': as) bs where
  type SetUnionT (a ': as) bs = a ': TU.Remove a (SetUnionT as bs)
  unionBulkUpdate 
    :: Either (NP f (a ': as)) (NP f bs)
    -> NP f (a ': TU.Remove a (SetUnionT as bs))
    -> NP f (a ': TU.Remove a (SetUnionT as bs))
  unionBulkUpdate (Right ys) (x :* zs) = x :* remove (Proxy @a) full_as_bs'
   where
    full_as_bs  = putback @_ @a @(SetUnionT as bs) x zs
    full_as_bs' = unionBulkUpdate @_ @as @bs (Right ys) full_as_bs
  unionBulkUpdate (Left (x :* xs)) (_ :* zs) = x :* remove (Proxy @a) full_as_bs'
   where
    full_as_bs  = putback @_ @a @(SetUnionT as bs) x zs
    full_as_bs' = unionBulkUpdate @_ @as @bs (Left xs) full_as_bs

