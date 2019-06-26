{-# LANGUAGE TypeFamilyDependencies #-}
module Answers2 where

import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.Prelude
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
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor material) = UnsafeMkDoor material

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor material) = UnsafeMkDoor material

mkDoor :: Door a -> Door b
mkDoor (UnsafeMkDoor material) = UnsafeMkDoor material

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor = mkDoor

closeSomeOpendDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpendDoor (MkSomeDoor s d) = case s of
  SOpened -> Just . fromDoor SClosed $ closeDoor d
  _       -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor sd@(MkSomeDoor s d) = case s of
  SOpened -> fromDoor SLocked . lockDoor $ closeDoor d
  SClosed -> fromDoor SLocked $ lockDoor d
  SLocked -> sd

-- | Types unknown until run Time

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor s material = case toSing s of
  SomeSing ss -> fromDoor ss (UnsafeMkDoor material)

-- | define existentially quantified type
-- These are types that are hidden to the user but directly chosen by the producer

-- | define universally quantified type
-- These are types that are chosen by the user

-- | define witheDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r

withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor s material f = case toSing s of
  SomeSing ss -> f ss (UnsafeMkDoor material)

-- | define reification
-- Is the lifting of dynamic run time values to the type level

-- | define SingKind instances for Maybe define a Maybe_ data type to avoid clashing with imported instances


{-
Let’s revisit our original redundant SomeDoor, compared to our final SomeDoor:

data OldSomeDoor :: Type where
    OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

To help convince yourself that the two are equal, write functions converting between the two:

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

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
    | n `mod` 2 == 1 = Just (UnsafeMkDoor m)
    | otherwise      = Nothing

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n d = case unlockDoor n d of
  Just d' -> fromDoor SClosed d'
  Nothing -> fromDoor SLocked d

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

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n sd@(MkSomeDoor s d) = case withSingI s (openAnyDoor n d) of
  Just od -> fromDoor SOpened od
  Nothing -> sd -- TODO: look at what gets returned

{--
Write the Sing and SingKind instance for the promoted kind of a custom list type:

data List a = Nil | Cons a (List a)

-}

data List a = LNil | LCons  a (List a)

data instance Sing (x :: List k) where
  SLNil :: Sing 'LNil
  SLCons :: Sing y -> Sing ys -> Sing ('LCons y ys)

instance SingKind k => SingKind (List k) where
  type Demote (List k) = List (Demote k)

  toSing :: List (Demote k) -> SomeSing (List k)
  toSing = \case
    LNil -> SomeSing SLNil
    LCons x xs -> withSomeSing x $ \sx ->
                  withSomeSing xs $ \ sxs ->
                     SomeSing $ SLCons sx sxs

  fromSing :: Sing (a :: List k) -> List (Demote k)
  fromSing = \case
    SLNil -> LNil
    SLCons sx sxs -> LCons (fromSing sx) (fromSing sxs)
