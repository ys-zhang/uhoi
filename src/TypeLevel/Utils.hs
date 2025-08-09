{-# LANGUAGE UndecidableInstances #-}
module TypeLevel.Utils where

import Data.Kind (Type)
import Data.SOP
import Data.Void
import Data.Type.Ord
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))

type family IsVoid (x :: Type) :: Bool where
  IsVoid Void = 'True
  IsVoid _ = 'False

-- ==================================================================================================================
-- Type-Level Operators: Defunctionalization  
-- ==================================================================================================================
type family UniOp (f :: op) (a :: k) :: k
type family BiOp (f :: op) (a :: k) (b :: k) :: k

type instance UniOp (p :.: q) a = UniOp p (UniOp q a)

-- | type-level list operators
type data OpList = OpListAppend | OpSort
type instance BiOp OpListAppend xs ys = xs ++ ys
type instance UniOp OpSort xs = Sort xs

-- ==================================================================================================================
--  Type-Level List Operations 
-- ==================================================================================================================

-- Basic type-level list concatenation
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Sort xs where
  Sort '[] = '[]
  Sort (x ': xs) = InsertSorted x (Sort xs)
type family InsertSorted x xs where
  InsertSorted x '[] = '[x]
  InsertSorted x (y ': ys) = OrdCond (Compare x y) 
                                     (x ': y ': ys)
                                     (x ': y ': ys)
                                     (y ': InsertSorted x ys)

type family MapList (f :: op) (xs :: [k]) :: [k] where
  MapList f '[] = '[]
  MapList f (x ': xs) = UniOp f x ': MapList f xs

type family ZipList (f :: op) (xs :: [k]) (ys :: [k]) :: [k] where
  ZipList f '[] _ = '[]
  ZipList f _ '[] = '[]
  ZipList f (x ': xs) (y ': ys) = BiOp f x y ': ZipList f xs ys

type family ReverseList (xs :: [k]) :: [k] where
  ReverseList '[] = '[]
  ReverseList (x ': xs) = ReverseList xs ++ '[x]

-- | Unique the type list, works fine on type aliases
type family Unique (xs :: [k]) :: [k] where
  Unique '[] = '[]
  Unique (x ': xs) = x ': Unique (Remove x xs)

type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove x '[] = '[]
  Remove x (y ': xs) = If (y == x) (Remove x xs) (y ': Remove x xs)

type family Union (xs :: [k]) (ys :: [k]) :: [k] where
  Union '[] ys = ys
  Union (x ': xs) ys = x ': Union xs (Remove x ys)

type family UnionAll (xss :: [[k]]) :: [k] where
  UnionAll '[] = '[]
  UnionAll (xs ': xss) = Union xs (UnionAll xss)

type family Map (f :: k1 -> k2) (xs :: [k1]) :: [k2] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- >>> :kind! Unique '[(), (), Either Bool (), Rst Bool ()]
-- Unique '[(), (), Either Bool (), Rst Bool ()] :: [*]
-- = '[(), Either Bool ()]

-- ==================================================================================================================
--  Type-Level Tuple Operations 
-- ==================================================================================================================

type family Fst (p :: (k1, k2)) :: k1 where
  Fst '(x, _) = x
type family Snd (p :: (k1, k2)) :: k2 where
  Snd '(_, y) = y
type family Swap (pair :: (k1, k2)) :: (k2, k1) where
  Swap '(x, y) = '(y, x)
type family MapTuple (f :: op) (p :: (k, k)) :: (k, k) where
  MapTuple f '(x, y) = '(UniOp f x, UniOp f y)
type family ZipTuple (f :: op) (p1 :: (k, k)) (p2 :: (k, k)) :: (k, k) where
  ZipTuple f '(a1, b1) '(a2, b2) = '(BiOp f a1 a2, BiOp f b1 b2)

-- ==================================================================================================================
--  Type-Level Boolean
-- ==================================================================================================================
