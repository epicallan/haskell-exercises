module Exercises2 where

import Data.Kind (Type)
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

-- | write below functions

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = error "do me"

-- | fromDoor_ :: SingI s => Door s -> SomeDoor

closeSomeOpendDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpendDoor = error "do me"

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor = error "do me"


-- | Types unknown until run Time

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = error "do me"

-- | define existentially quantified type

-- | define universally quantified type

-- | define witheDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r

-- | define reification

-- | define SingKind_ class which is a mirror of SingKind

-- | define SingKind_ instances for Maybe


{-
Let’s revisit our original redundant SomeDoor, compared to our final SomeDoor:

View full source
data OldSomeDoor :: Type where
    OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

To help convince yourself that the two are equal, write functions converting between the two:

View full source
toOld :: SomeDoor -> OldSomeDoor

fromOld :: OldSomeDoor -> SomeDoor

Avoid directly pattern matching on the singletons or constructors. Instead, use singletons library tools like toSing, withSomeSing, fromSing, etc.

-}


{-
 Previously, we had an unlockDoor function that took an Int (the “password”) with a Door 'Locked and returned a Maybe (Door 'Closed). It returns a Door 'Closed (unlocked door) in Just if an odd number was given, and Nothing otherwise (a failed unlock)

Use this to implement a that would return a SomeDoor. Re-use the “password” logic from the original unlockDoor. If the door is successfully unlocked (with a Just), return the unlocked door in a SomeDoor. Otherwise, return the original locked door (in a SomeDoor).

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
    | n `mod` 2 == 1 = Just (UnsafeMkDoor m)
    | otherwise      = Nothing

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor = ???
-}

{-
 Implement openAnyDoor' in the same style, with respect to openAnyDoor:

View full source
openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor = ???

Remember to re-use openAnyDoor.
-}

{--
Write the SingKind instance for the promoted kind of a custom list type:

View full source
data List a = Nil | Cons a (List a)

data instance Sing (x :: List k) where
    SNil  :: Sing 'Nil
    SCons :: Sing x -> Sing xs -> Sing ('Cons x xs)

instance SingKind k => SingKind (List k) where
    type Demote (List k) = ???

    fromSing :: Sing (xs :: List k) -> List (Demote k)
    fromSing = ???

    toSing :: List (Demote k) -> SomeSing (List k)
    toSing = ???

The singletons for List are:

SNil  :: Sing 'Nil
SCons :: Sing x -> Sing xs -> Sing ('Cons x xs)

Note that the built-in singletons for the list type also uses these same constructor names, for [] and :.
-}
