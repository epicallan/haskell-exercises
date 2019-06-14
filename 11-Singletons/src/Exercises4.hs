-- | answers to section 4 questions in Singletons part 4 series
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercises4 where

import Data.Kind
import Data.Singletons
import Data.Singletons.Prelude hiding (And, Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Or,
                                sFoldr)
-- import Data.Singletons.Sigma
import Data.Singletons.TH hiding (Fold, Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFold,
                           sFoldr)


$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

{-
Write mergeDoor; What will the new state door state be ?


mergeDoor :: Door s -> Door t -> Door ????
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e
-}

{-
write:  mergeSomeDoor :: SomeDoor -> SomeDoor ->SomeDoor
-}

{-
Let’s make a data type that represents a series of hallways, each linked by a door. A hallway is either an empty stretch with no door, or two hallways linked by a door. We’ll structure it like a linked list, and store the list of all door states as a type-level list as a type parameter:

write Hallway type

such that

ghci> let door1 = mkDoor SClosed "Oak"
ghci> let door2 = mkDoor SOpened "Spruce"
ghci> let door3 = mkDoor SLocked "Acacia"
ghci> :t door1 :<# door2 :<# door3 :<# HEnd

Hallway '[ 'Closed, 'Opened, 'Locked ]

-}

-- | write function to collapse all doors in a hallway
-- collapseHallway :: Hallway ss -> Door ?????

{-
 write type definition for  a ~> b
-}

{-
 write global interpreter type family definition for defunctionalisation symbol a ~> a
-}
{-
 Given
 data Id :: a ~> a
 data Not :: Bool ~> Bool
 write an iterpreter instances.
-}

{-
$(singletons [d|
  and :: Bool -> (Bool -> Bool)
  and False _ = False
  and True  x = x
  ])

write the defunctionalization symbols created by singleton function above
-}

{--
 what is the returned value of below kind functions

ghci> :kind! AndSym0 @@ 'False

ghci> :kind! AndSym1 'False @@ 'True

ghci> :kind! AndSym1 'True  @@ 'True

-}

{-
 we can lift any type constructor into a “free” defunctionalization symbol with TyCon1
 write definition for TyCon1 and its Apply (@@) instance
-}

{-
  write Maybe Int in terms of TyCon1
  TyCon1 ??? = Maybe Int
-}

{-
 write Foldr type family in terms of defunctionalization symbols

 write a MergeSateList in terms of Foldr
-}

{-
 define a sing monoid isntance for DoorState in terms of mergeState
-}

{-
 Promote fold as a sing function
-}

{-
write a collapseSomeHallwat function that makes use of the promoted fold
-}

-- | define what a dependent pair is

-- | write definition for Sigma type

-- | define SomeDoor in terms of Sigma

-- | define SomeHallway as a dependentapair over [DoorState] with Hallway

-- exercises ??
