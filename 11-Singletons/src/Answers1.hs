{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
module Answers1 where

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

-- | Given below types

data Door (s :: DoorState) where
  UnsafeMkDoor ::  { doorMaterial :: String }  -> Door s

data DoorState = Opened | Closed | Locked

-- | derive Singleton instances for DoorState i.e Sing, SingI, SingKind

data family Sing :: k -> Type

data instance Sing (k :: DoorState) where
  SOpened :: Sing 'Opened
  SClosed :: Sing 'Closed
  SLocked :: Sing 'Locked

class SingI (a :: k) where
  sing :: Sing a

instance SingI 'Opened where
  sing = SOpened

instance SingI 'Closed where
  sing = SClosed

instance SingI 'Locked where
  sing = SLocked

class SingKind k  where      -- `k` is a kind!
  -- | Associate a kind k with its reflected type
  type Demote k = (r :: Type) | r -> k

  fromSing :: Sing (a :: k)  -> Demote k

  toSing :: Demote k -> SomeSing k

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

instance SingKind DoorState where
  type Demote DoorState = DoorState

  fromSing :: Sing (a :: DoorState) -> DoorState
  fromSing  = \case
    SOpened -> Opened
    SClosed -> Closed
    SLocked -> Locked

  toSing :: DoorState -> SomeSing DoorState
  toSing = \case
    Opened -> SomeSing SOpened
    Closed -> SomeSing SClosed
    Locked -> SomeSing SLocked

-- | A 'SingInstance' wraps up a 'SingI' instance for explicit handling.
data SingInstance (a :: k) where
  SingInstance :: SingI a => SingInstance a


-- dirty implementation of explicit-to-implicit conversion
newtype DI a = DonTInstantiate (SingI a => SingInstance a)

-- | Get an implicit singleton (a 'SingI' instance) from an explicit one.
singInstance :: forall k (a :: k). Sing a -> SingInstance a
singInstance s = withSingInstance SingInstance
  where
    withSingInstance :: (SingI a => SingInstance a) -> SingInstance a
    withSingInstance si = unsafeCoerce (DonTInstantiate si) s

withSingI :: forall k r (a :: k). Sing a -> (SingI a => r) -> r
withSingI sn r = case singInstance sn of
  SingInstance -> r

withSing :: forall k r (a :: k). SingI a => (Sing a -> r) -> r
withSing f  = f sing

withSomeSing :: forall k r
             . SingKind k
             => Demote k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing value f = case toSing value of
  SomeSing sn -> f sn


-- | write below functions

-- | lockAnyDoor :: Sing s -> Door s -> Door 'Locked

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockAnyDoor :: forall s. Sing s -> Door s -> Door 'Locked
lockAnyDoor sings door = case sings of
  SOpened -> lockDoor . closeDoor $ door
  SClosed -> lockDoor door
  SLocked -> door

-- | explain what reflection is
-- reflection is the moving of values from runtime to type level

-- | doorStatus :: Sing s -> Door s -> DoorState
doorStatus :: Sing s -> Door s -> DoorState
doorStatus singD _ = fromSing singD

-- | lockAnyDoor_ :: SingI s => Door s -> Door 'Locked

lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

-- | write  lockAnyDoor_ Sing CPS style

lockAnyDoor__ :: SingI s => Door s -> Door 'Locked
lockAnyDoor__ d = withSing $ \sn -> lockAnyDoor sn d

-- | mkDoor :: Sing s -> String -> Door s
mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n door = if isOdd
  then Just $ mkDoor SClosed (doorMaterial door)
  else Nothing
    where
      isOdd = n `mod` 2 /= 0


-- | Write a function that can open any door, taking a password, in “implicit Sing” style:
-- openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
-- This should be written in terms of unlockDoor and openDoor (see above) – that is, you should not use UnsafeMkDoor directly for openAnyDoor.
-- If the door is already unlocked or opened, it should ignore the Int input.


openAnyDoor :: Sing s -> Door s -> Maybe (Door 'Opened)
openAnyDoor sn d  = case sn of
  SOpened -> Just d
  SClosed -> Just . openDoor $ d
  SLocked -> openDoor <$> unlockDoor 1 d
