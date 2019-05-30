module Exercises3 where

import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH

-- | Given below types

newtype Door s = UnsafeMkDoor { doorMaterial :: String } deriving Show

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

-- | Dependently Typed Proofs

-- | define a value level predicate

-- | define a type level predicate

-- | define a predicate Knockable :: DoorState -> Type as a GADT that only has values if
-- give 'Closed and 'Locked,  but not 'Opened


-- | write a knock function that required a proof that s is Knockable
-- knock :: Knockable s -> Door s -> IO ()

-- | Auto generating proofs with Proved class

class Proved p a where
  auto :: p a

-- | write Proved Knockable instances
-- such that
-- knockIO :: IO ()
-- knockIO = knock auto someDoor is valid

-- | Decidable predicates

{-

data Decision a = Proved a                  -- ^ a value of a exists
                | Disproved (Refuted a)     -- ^ a value of a cannot exist

-- | The data type with no values
data Void

-- | 'a' cannot exist.  Commonly also called `Not`
type Refuted a = a -> Void

-}

-- |  write isKnockable :: Sing s -> Decision (Knockable s)

-- | write knockSomeDoor :: SomeDoor -> IO ()


-- | Write SDecide class definition

-- | write Bool instance for SDecide

-- | given below types

$(singletons [d|
  data Pass = Obstruct | Allow
  |])

-- | define type family StatePass (s :: DoorState) :: Pass

-- | define a knock function that uses StatePass

-- | define KnockSomeDoor while using Pass

-- | Promoted Typeclasses

-- | For class Eq write promoted Versions PEq and SEq
{-

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])

write singleton Eq instances that would be provided for Pass by TH
-}


{-
We talk about predicates as type constructors with type k -> Type. This fits a lot of things we’ve seen before (all instances of Functor, for example), but some predicates are more interesting than others.

What is the interpretation of SDoorState as a predicate? (remember, SDoorState s is the type synonym for Sing (s :: DoorState)) What “traditional” (that is, a -> Bool) predicate does it correspond to?

What is the type of its decision function? Can you implement it?

-}

{-
Now let’s practice working with predicates, singletons, and negation via Refuted together.

You may have heard of the principle of “double negation”, where not (not p) implies p. So, we should be able to say that Refuted (Refuted (Knockable s)) implies Knockable s.8 If something is not “not knockable”, then it must be knockable, right?

Try writing refuteRefuteKnockable to verify this principle — at least for the Knockable predicate.

View full source
refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))
    -> Knockable s

While not required, I recommend using isKnockable and writing your implementation in terms of it! Use sing to give isKnockable the singleton it needs.

Hint: You might find absurd (from Data.Void) helpful:

absurd :: forall a. Void -> a

If you have a Void, you can make a value of any type!9
-}

{-

(This next one is fairly difficult compared to the others, and is only tangentially related to singletons, so feel free to skip it!)

Type-level predicates are logical constructs, so we should be able to define concepts like “and” and “or” with them.

    Define a predicate constructor And that takes two predicates and returns a new predicate. This new predicate is true (aka, has an inhabitant) if and only if the two original predicates are true (aka, have inhabitants)

    View full source
    data And :: (k -> Type) -> (k -> Type) -> (k -> Type) where

    Define a predicate constructor Or that takes two predicates and returns a new predicate. This new predicate is true (aka, has an inhabitant) if and only if at least one of the two original predicates are true (aka, have inhabitants)

    View full source
    data Or :: (k -> Type) -> (k -> Type) -> (k -> Type) where

    There are potentially multiple non-trivial variations of this type.

    Do And and Or look similar to any types you might have encountered in the past? Maybe, perhaps, similiar to types that are a part of basic beginner Haskell concepts?

    Maybe surprisingly, And p q and Or p q are decidable if p and q are. Can we write the decision functions?

    View full source
    decideAnd
        :: (forall x. Sing x -> Decision (p x))
        -> (forall x. Sing x -> Decision (q x))
        -> Sing a
        -> Decision (And p q a)

    decideOr
        :: (forall x. Sing x -> Decision (p x))
        -> (forall x. Sing x -> Decision (q x))
        -> Sing a
        -> Decision (Or p q a)

    These functions actually demonstrate, I feel, why Decision having both a Proved a and Disproved (Refuted a) branch is very useful. This is because, if you wrote the structure of And and Or correctly, it’s impossible to incorrectly define decideAnd and decideOr. You can’t accidentally say false when it’s true, or true when it’s false — your implementation is guarunteed correct.

    Now let’s use And and Or to prove some useful facts about Knockable and ('Opened :~:). We know that it’s impossible for something to be both Knockable and ('Opened :~:) (that is, both knockable and equal to 'Opened). Write such a witness:

    View full source
    knockableNotOpened
        :: forall s. SingI s
        => Refuted (And Knockable ((:~:) 'Opened) s)

    We also know that a given DoorState is either Knockable or ('Opened :~:) — at least one of these is always true. Write such a witness:

    View full source
    knockableOrOpened
        :: forall s. SingI s
        => Or Knockable ((:~:) 'Opened) s

Solutions available here!

-}

{-
 Instead of creating an entire Knocked type, we could have just said “as long as the door is not 'Opened, you can knock”. This means we could write knock as:

knock :: Refuted (s :~: 'Opened) -> Door s -> IO ()

Which we must pass a proof that s is not equal to 'Opened in order to open our door.

Is this really the same thing? Is Refuted (s :~: 'Opened) the same thing as Knockable s?

Let’s try to say that the two things are the same! Write the following functions to show that Refuted (s :~: 'Opened) is the same logical predicate as Knockable s!

View full source
knockedRefute
    :: forall s. SingI s
    => Knockable s
    -> Refuted (s :~: 'Opened)

refuteKnocked
    :: forall s. SingI s
    => Refuted (s :~: 'Opened)
    -> Knockable s

Solution available here!

Note: knockedRefute is fairly straightforward, but refuteKnocked is definitely trickier, so don’t be discouraged!

Hint: See the note about absurd from Exercise 2!

-}
{-
5. On our type level function version of knock, we wrote, with a constraint:

knock :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

We can muddy the waters a bit, for fun, by having this take a proof of the constraint instead:

View full source
knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

Rewrite a version of knockSomeDoor in terms of knockRefl, called knockSomeDoorRefl:

View full source
knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =

Remember not to use knock!

Solution available here.

Assume that DoorState has an instance of SDecide, so you can use (%~). This should be derived automatically as long as you derive Eq:

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])


-}

{-
With the function that inverts Pass:

$(singletons [d|
  invertPass :: Pass -> Pass
  invertPass Obstruct = Allow
  invertPass Allow    = Obstruct
|])

Implement knock in a way that lets you knock if invertPass is Allow:

View full source
knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

And write knockSomeDoor in terms of it:

View full source
knockSomeDoorInv
    :: SomeDoor
    -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =

Remember again to implement it in terms of knockInv, not knock.

Solution available here!
-}

{-
Let’s work with a toy typeclass called Cycle, based on Enum

$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])

next is like succ, but loops over to the first item after the last constructor. prev is like pred, but loops over to the last item if pred-ing the first item

View full source
instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed

Can you manually promote this instance for DoorState to the type level?

View full source
instance PCycle DoorState where

instance SCycle DoorState where

Solution available here!
-}