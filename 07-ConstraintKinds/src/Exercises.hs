{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Exercises where -- ^ This is starting to look impressive, right?

import Data.Kind (Constraint, Type)

-- | Just a quick one today - there really isn't much to cover when we talk
-- about ConstraintKinds, as it's hopefully quite an intuitive extension: we're
-- just extending the set of things that we can use as constraints to include
-- type parameters!





{- ONE -}

-- | Here's a super uninteresting list, which comes with an unfortunate
-- restriction: everything in the list must have the same exact type.

data List a = Nil | Cons a (List a)

-- | We can generalise this data structure to a /constrained list/, in which we
-- say, instead of "every value has this type", we say, "every value's type
-- satisfies this constraint".

-- | a. Do it! Think about the @Nil@ and @Cons@ cases separately; which
-- constraints can the @Nil@ case satisfy?

data ConstrainedList (c :: Type -> Constraint) :: Type where
  NilC :: ConstrainedList c
  ConsC :: forall c a. c a => a -> ConstrainedList c -> ConstrainedList c

data IdNum = IdNum Int deriving Show

sampleC :: ConstrainedList Show
sampleC = ConsC (IdNum 10) ( ConsC "hey" NilC)

-- | b. Using what we know about RankNTypes, write a function to fold a
-- constrained list. Note that we'll need a folding function that works /for
-- all/ types who implement some constraint @c@. Wink wink, nudge nudge.

foldConstrainedList :: forall m c. Monoid m => (forall a. c a => a -> m) -> ConstrainedList c -> m
foldConstrainedList  _ NilC         = mempty
foldConstrainedList fn (ConsC x xs) = fn x <> foldConstrainedList fn xs

-- foldr :: (a -> b -> b) -> b -> [a] -> b
foldrConstrainedList :: (forall a. c a => (a -> b -> b)) -> b -> ConstrainedList c -> b
foldrConstrainedList _ b NilC            = b
foldrConstrainedList fn b (x `ConsC` xs) = x `fn` foldrConstrainedList fn b xs

-- | Often, I'll want to constrain a list by /multiple/ things. The problem is
-- that I can't directly write multiple constraints into my type, because the
-- kind of @(Eq, Ord)@ isn't @Type -> Constraint@ - it's actually a kind error!

-- | There is hope, however: a neat trick we can play is to define a new class,
-- whose super classes are all the constraints we require. We can then write an
-- instance for any a who satisfies these constraints. Neat, right?

-- | c. Write this class instance so that we can have a constraint that
-- combines `Monoid a` and `Show a`. What other extension did you need to
-- enable? Why?

class (Semigroup a, Monoid a, Show a) => Constraints a

-- UndecidableInstances is required because nothing is getting "smaller" - when
-- GHC encounters this constraint, it ends up with more things to solve! This
-- means that GHC's (limited) termination checker can't be convinced that we'll
-- ever terminate.
instance (Semigroup a, Monoid a, Show a) => Constraints a


sampleB :: ConstrainedList Constraints
sampleB = ConsC (mempty :: String) NilC

-- | What can we now do with this constrained list that we couldn't before?
-- There are two opportunities that should stand out!





{- TWO -}

-- | Recall our HList:

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Ideally, we'd like to be able to fold over this list in some way, ideally
-- by transforming every element into a given monoid according to the interface
-- of some constraint. To do that, though, we'd need to know that everything in
-- the list implemented a given constraint... if only we had a type family for
-- this...

-- | a. Write this fold function. I won't give any hints to the definition, but
-- we will probably need to call it like this:

-- test :: ??? => HList xs -> String
-- test = fold (TCProxy :: TCProxy Show) show

-- | b. Why do we need the proxy to point out which constraint we're working
-- with?  What does GHC not like if we remove it?

-- | We typically define foldMap like this:

foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap fn = foldr (\x acc -> fn x <> acc) mempty

-- | c. What constraint do we need in order to use our @(a -> m)@ function on
-- an @HList@? You may need to look into the __equality constraint__ introduced
-- by the @GADTs@ and @TypeFamilies@ extensions, written as @(~)@:

    -- * This tells GHC that @a@ and @b@ are equivalent.
f :: a ~ b => a -> b
f = id

-- | Write @foldMap@ for @HList@!
