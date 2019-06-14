{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Answers where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (x :: Nat) + (y :: Nat) :: Nat where
  'Z   + y =         y
  'S x + y = 'S (x + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

infixr 7 **

type family (x :: Nat) ** (y :: Nat) :: Nat where
  'Z   ** y = 'Z
  'S x ** y = y + (x ** y)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.

add :: SNat x -> SNat y -> SNat (x + y)
add  SZ    y =           y
add (SS x) y = SS (add x y)


{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil xs        = xs
append (VCons x xs) y = VCons x (append xs y)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.


flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n **  m) b
flatMap VNil _         = VNil
flatMap (VCons x xs) f = append (f x) (flatMap xs f)


{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

-- | b. Write the type-level @||@ function for booleans.

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family (&&) (x :: Bool) (y :: Bool) :: Bool where
  'True && y = y
  'False && x = 'False

type family (||) (x :: Bool) (y :: Bool) :: Bool where
  'True || y = 'True
  'False || y = y

type family All (x :: [Bool]) :: Bool where
  All '[ ] = 'True
  All (x ': xs) = x && All xs

{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

-- data Order = GT | LT

type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare 'Z 'Z = 'EQ
  Compare ('S x) 'Z = 'GT
  Compare ('Z) ('S x) = 'LT
  Compare ('S x) ('S y) = Compare x y

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max x y = Max' (Compare x y) x y

type family Max' (o :: Ordering) (x :: Nat) (y :: Nat) :: Nat where
  Max' 'LT _ y = y
  Max' _ x _ = x

-- | c. Write a family to get the maximum natural in a list.

type family Maximum (xs :: [Nat]) :: Nat where
  Maximum '[ ] = 'Z
  Maximum (x ': xs) = Max x (Maximum xs)

{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family Insert (x :: Nat) (xs :: Tree) :: Tree where
  Insert x  'Empty       = 'Node 'Empty x 'Empty
  Insert x ('Node l c r) = Insert' (Compare x c) x ('Node l c r)

type family Insert' (o :: Ordering) (x :: Nat) (xs :: Tree) :: Tree where
  Insert' 'LT x ('Node l c r) = 'Node (Insert x l) c r
  Insert' 'GT x ('Node l c r) = 'Node l c (Insert x r)
  Insert' 'EQ x  xs           =  xs

{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.



{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.

type family (++) (xs :: [Type]) (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ':  (xs ++ ys)

appendHList :: HList xs -> HList ys -> HList (xs ++ ys)
appendHList HNil ys           = ys
appendHList (x `HCons` xs) ys = x `HCons` ( appendHList xs ys)


{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every c '[ ] = ()
  Every c (x ': xs ) = (c x, Every c xs)


-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

instance (Every Show xs) => Show (HList xs) where
  show HNil         = "HNil"
  show (HCons x xs) = show x ++ ":" ++ show xs

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance Every Eq xs => Eq (HList xs) where
  HCons x xs == HCons y ys = x == y && xs == ys
  _          == _          = True -- Could only be two HNils!

-- We have to add 'Every Eq xs' here, which may seem odd, as 'Ord' has
-- previously /implied/ 'Eq'. If GHC knows that every element has an 'Ord'
-- instance, why can't it tell that every one has an 'Eq' instance? The reason
-- is that it certainly /could/ if it tried a bit harder. GHC can't /see/ a
-- constraint that says any particular @x@ has an 'Ord' constraint, which means
-- it can't convince itself that this will be true in the general case.
instance (Every Eq xs, Every Ord xs) => Ord (HList xs) where
  compare (HCons x xs) (HCons y ys) = compare x y <> compare xs ys
  compare  _            _           = EQ




{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
