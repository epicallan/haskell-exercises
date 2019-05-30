module Exercises1 where

-- | Given below types

newtype Door s = UnsafeMkDoor { doorMaterial :: String } deriving Show

data DoorState = Opened | Closed | Locked


-- | derive Singleton instances for DoorState i.e Sing, SingI, SomeSing & SingKind 

-- | write below functions

-- | lockAnyDoor :: Sing s -> Door s -> Door 'Locked

-- | explain what reflection is

-- | fromSing :: Sing s -> DoorState

-- | doorStatus :: Sing s -> Door s -> DoorState

-- | lockAnyDoor_ :: SingI s => Door s -> Door 'Locked

-- | write the withSingI CPS like function

-- | write lockAnyDoor with "withSingI"

-- | mkDoor :: Sing s -> String -> Door s


-- | Write a function to unlock a door, but only if the user enters an odd number (as a password).
-- unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)


-- | Write a function that can open any door, taking a password, in “implicit Sing” style:
-- openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
-- This should be written in terms of unlockDoor and openDoor (see above) – that is, you should not use UnsafeMkDoor directly for openAnyDoor.
-- If the door is already unlocked or opened, it should ignore the Int input.
