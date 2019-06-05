module Exercises1 where

-- | Given below types
data Door (s :: DoorState) where
  UnsafeMkDoor ::  { doorMaterial :: String }  -> Door s

data DoorState = Opened | Closed | Locked


-- | derive Singleton instances for DoorState i.e Sing, SingI, SomeSing & SingKind

-- | write these singleton functions:  withSingI, withSing, withSomeSing

-- | write below functions

-- | lockAnyDoor :: Sing s -> Door s -> Door 'Locked

-- | explain what reflection is

-- | doorStatus :: Sing s -> Door s -> DoorState

-- | lockAnyDoor_ :: SingI s => Door s -> Door 'Locked

-- | write lockAnyDoor in Sing CPS style

-- | mkDoor :: Sing s -> String -> Door s


-- | Write a function to unlock a door, but only if the user enters an odd number (as a password).
-- unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Opened)


-- | Write a function that can open any door, taking a password, in “implicit Sing” style:
-- openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
-- This should be written in terms of unlockDoor and openDoor (see above) – that is, you should not use UnsafeMkDoor directly for openAnyDoor.
-- If the door is already unlocked or opened, it should ignore the Int input.
