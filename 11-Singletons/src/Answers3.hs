{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Answers3 where

import Data.Kind (Type)
import Data.Void
import Data.Singletons
import Data.Singletons.TH

-- | Given below types

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

newtype Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String } deriving Show

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

-- | Dependently Typed Proofs

-- | define a value level predicate
-- A value level predicate is (a -> Bool)

-- | define a type level predicate
-- is a type constructor of kind (k -> Type) . Given a type of kind k if a value of that Type
-- can exist then the predicate is satisfied

-- | define a predicate Knockable :: DoorState -> Type as a GADT that only has values if
-- give 'Closed and 'Locked,  but not 'Opened

data Knockable (a :: DoorState) :: Type where
  KnockableClosed :: Knockable 'Closed
  KnockableLocked :: Knockable 'Locked

-- | write a knock function that requires a proof that s is Knockable

knock :: Knockable s -> Door s -> IO ()
knock _ door = putStrLn $ "door is knockable" <> doorMaterial door

-- | Auto generating proofs with Proved class

class Proved p a where
  auto :: p a

instance Proved Knockable 'Closed where
  auto = KnockableClosed

instance Proved Knockable 'Locked where
  auto = KnockableLocked

-- | write Proved Knockable instances
-- such that
-- knockIO :: IO ()
-- knockIO = knock auto someDoor is valid

knockIO :: IO ()
knockIO = knock auto someDoor
  where
    someDoor :: Door 'Locked
    someDoor = UnsafeMkDoor "Oak"

-- | Decidable predicates, write definitions for Decision 


-- |  write isKnockable :: Sing s -> Decision (Knockable s)

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
  SLocked -> Proved KnockableLocked
  SClosed -> Proved KnockableClosed
  SOpened -> Disproved $ \case {}

-- | write knockSomeDoor :: SomeDoor -> IO ()

knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
  Proved knockable -> knock knockable d
  Disproved _ -> putStrLn "not allowed"

-- | Write SDecide class definition

{-
 data :~: :: k1 -> k2 -> Type where
   Refl :: a :~: a

 class SDecide k where
  (%~) :: Sing (a :: k) -> Sing (b :: k) -> Decision (a :~: b)
-}

-- | write Bool instance for SDecide


-- | given below types

$(singletons [d|
  data Pass = Obstruct | Allow
      deriving (Eq)
  |])

-- | define type family StatePass (s :: DoorState) :: Pass

type family StatePass (s :: DoorState) :: Pass where
  StatePass 'Opened = 'Allow
  StatePass 'Closed = 'Obstruct
  StatePass 'Locked = 'Obstruct

-- | define a knock function that uses StatePass

knock_ :: forall s. (StatePass s ~ 'Obstruct) => Door s -> IO ()
knock_ (UnsafeMkDoor material)=  putStrLn $ "knock on door" <> material

-- | define KnockSomeDoor while using Pass

statePass :: Sing s -> Sing (StatePass s)
statePass = \case
  SOpened -> SAllow
  SClosed -> SObstruct
  SLocked -> SObstruct

knockSomeDoor_ :: SomeDoor -> IO ()
knockSomeDoor_ (MkSomeDoor s d) = case statePass s of
  SAllow -> putStrLn "Can't knock"
  SObstruct -> knock_ d

-- | Promoted Typeclasses

-- | For class Eq write promoted Versions PEq and SEq
{-
 class Eq a where
   (==) a -> a :: Bool
   (/=) a -> a :: Bool

 class PEq k where
   type (==) (a :: k) (b :: k) :: Bool
   type (/=) (a :: k) (b :: k) :: Bool

 class SEq k where
  (%==) :: Sing (a :: k) -> Sing (b :: k) -> Sing (a == b)
  (%/=) :: Sing (a :: k) -> Sing (b :: k) -> Sing (a /= b)
-}

{-

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])

write singleton Eq instances that would be provided for Pass by TH

instance SEq Pass where
  SObstruct %== SObstruct = 'True
 ...

-}



{-
Now let’s practice working with predicates, singletons, and negation via Refuted together.

You may have heard of the principle of “double negation”, where not (not p) implies p. So, we should be able to say that Refuted (Refuted (Knockable s)) implies Knockable s . If something is not “not knockable”, then it must be knockable, right?

Try writing refuteRefuteKnockable to verify this principle — at least for the Knockable predicate.


refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))
    -> Knockable s

While not required, I recommend using isKnockable and writing your implementation in terms of it! Use sing to give isKnockable the singleton it needs.

Hint: You might find absurd (from Data.Void) helpful:

absurd :: forall a. Void -> a

If you have a Void, you can make a value of any type!
-}

refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))
    -> Knockable s
refuteRefuteKnockable rrK =
    case isKnockable (sing @s) of   -- sing @_ @s for singletons-2.4.1 and earlier
      Proved    k  -> k
      Disproved rK -> absurd (rrK rK)

{-
 Instead of creating an entire Knocked type, we could have just said “as long as the door is not 'Opened, you can knock”. This means we could write knock as:

knock :: Refuted (s :~: 'Opened) -> Door s -> IO ()

Which we must pass a proof that s is not equal to 'Opened in order to open our door.

Is this really the same thing? Is Refuted (s :~: 'Opened) the same thing as Knockable s?

Let’s try to say that the two things are the same! Write the following functions to show that Refuted (s :~: 'Opened) is the same logical predicate as Knockable s!

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

knockedRefute
    :: forall s. SingI s
    => Knockable s
    -> Refuted (s :~: 'Opened)
knockedRefute = \case
  KnockableClosed -> \case {}
  KnockableLocked -> \case {}

refuteKnocked
    :: forall s. SingI s
    => Refuted (s :~: 'Opened) -- \a -> Void
    -> Knockable s
refuteKnocked v = case sing @s of
  SOpened -> absurd $ v (Refl @'Opened) -- absurd :: Void -> a
  SClosed -> KnockableClosed
  SLocked -> KnockableLocked


knockIO_ :: Door 'Closed -> IO ()
knockIO_ door = knock (refuteKnocked isKnockable_) door
  where
    isKnockable_ :: Refuted ('Closed :~: 'Opened)
    isKnockable_ = \case {}

{-
5. On our type level function version of knock, we wrote, with a constraint:

knock :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

We can muddy the waters a bit, for fun, by having this take a proof of the constraint instead:

knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

-}

knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockReflIO :: IO ()
knockReflIO = knockRefl (Refl @(StatePass 'Closed)) door
  where
    door :: Door 'Closed
    door = UnsafeMkDoor "OAK"
{-
Rewrite a version of knockSomeDoor in terms of knockRefl, called knockSomeDoorRefl:

knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =

Remember not to use knock!


Assume that DoorState has an instance of SDecide, so you can use (%~). This should be derived automatically as long as you derive Eq:

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])


-}

knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =
    case statePass s %~ SObstruct of
      Proved r    -> knockRefl r d
      Disproved _ -> putStrLn "No knocking allowed!"

{-
With the function that inverts Pass:

$(singletons [d|
  invertPass :: Pass -> Pass
  invertPass Obstruct = Allow
  invertPass Allow    = Obstruct
|])

-}

type family InvertPass (x :: Pass) :: Pass where
  InvertPass 'Obstruct = 'Allow
  InvertPass 'Allow = 'Obstruct

invertPass :: forall (a :: Pass). Sing a -> Sing (InvertPass a)
invertPass = \case
  SObstruct -> SAllow
  SAllow -> SObstruct

{-

Implement knock in a way that lets you knock if invertPass is Allow:

knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

And write knockSomeDoor in terms of it:

knockSomeDoorInv
    :: SomeDoor
    -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =

Remember again to implement it in terms of knockInv, not knock.

-}

knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorInv
    :: SomeDoor
    -> IO ()
knockSomeDoorInv (MkSomeDoor s d) = case statePass s of
  SAllow -> putStrLn "not allowed"
  SObstruct -> knockInv d


{-
Let’s work with a toy typeclass called Cycle, based on Enum

$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])

next is like succ, but loops over to the first item after the last constructor. prev is like pred, but loops over to the last item if pred-ing the first item

instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed

Can you manually promote this instance for DoorState to the type level?

instance PCycle DoorState where

instance SCycle DoorState where

-}

$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])


instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed

instance PCycle DoorState where
  type Next 'Opened = 'Closed
  type Next 'Closed = 'Locked
  type Next 'Locked = 'Opened

  type Prev 'Opened = 'Locked
  type Prev 'Closed = 'Opened
  type Prev 'Locked = 'Closed

instance SCycle DoorState where
  sNext = \case
    SOpened -> SClosed
    SClosed -> SLocked
    SLocked -> SOpened

  sPrev = \case
    SOpened -> SLocked
    SClosed -> SOpened
    SLocked -> SClosed
