{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exercises1 where

import Data.Kind
-- import Data.Singletons
import Data.Singletons.TH
import Prelude hiding (replicate, (++))

{-
So, the (a?) problem with TypeNats from GHC is that it has no internal structure.
It’s basically the same as the Integer or Natural type — every single value (constructor)
is completely structurally unrelated to the next.

To add structure we use inductive type level nats
-}

-- | Define a length indexed vector using a structured Nat type
$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

infixr 8 :>>

data Vector :: Nat -> Type -> Type where
   VNil :: Vector 'Z a
   (:>>) :: a -> Vector n a -> Vector ('S n) a

-- | define an fmap function

vmap :: (a -> b) -> Vector m a -> Vector m b
vmap f = \case
  VNil -> VNil
  (x :>> xs) -> f x :>> vmap f xs

-- | define a zip function

vzip :: Vector n a -> Vector n b -> Vector n (a, b)
vzip = \case
  VNil -> const VNil
  (x :>> xs) -> \case
    (y :>> ys) -> (x, y) :>> vzip xs ys

-- | define an append function

type family (++) (a :: Nat) (b :: Nat) :: Nat where
  x ++ 'Z = x
  x ++ 'S y = 'S ( x ++ y)

vappend :: Vector n a -> Vector m a -> Vector (n ++ m) a
vappend x VNil       = x
vappend x (y :>> ys) = y :>> x `vappend` ys

-- | define a type safe index function

data Fin (n :: Nat) :: Type where
  FZ :: Fin ('S 'Z)
  FS :: Fin n -> Fin ('S n)

-- Fin ('S 'Z)  -> Vector ('S 'S 'Z) (a)
-- (FS FZ) (x :>> xs) =

vindex :: Fin n -> Vector n a -> a
vindex FZ (x :>> _)      = x
vindex (FS i) (_ :>> xs) = vindex i xs

--  | define  withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r
withVec :: [a] -> (forall n. Sing n -> Vector n a -> r) -> r
withVec xs f = case xs of
  [] -> f SZ VNil
  (x' : xs') -> withVec xs' $ \s v ->
                   f (SS s) (x' :>> v)



-- define exactLength_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLength_ :: Sing m  -> Vector n a -> Maybe (Vector m a)
exactLength_ sn vec = case vlength vec %~ sn of
    Proved Refl -> Just vec
    Disproved _ -> Nothing

vlength :: Vector n a -> Sing n
vlength VNil       = SZ
vlength (_ :>> xs) = SS ( vlength xs)

-- define exactLength :: SingI m => Vec n a -> Maybe (Vec m a)

{-
 For example, we can make a witness that n is less than or equal to m,
 as well as a way to construct such a witness:

 Define that witness
-}

--- data LTE ...

-- define proove for witness

-- define atLeast_ :: Sing n -> Vec m a -> Maybe (LTE n m, Vec m a)


-- define atLeast :: SingI n => Vec m a -> Maybe (LTE n m, Vec m a)



{-
We can write a function that can “take” an arbitrary amount from a vector,
given (via proof) that the vector has at least that many elements:
-}

-- define takeVec :: LTE n m -> Vec m a -> Vec n a

-- define takeVecMaybe_ :: Sing n -> Vec m a -> Maybe (Vec n a)
