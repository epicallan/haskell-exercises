module Answers where


import Data.Kind (Type)

-- | Fix the following classes by annotating them with kind signatures. GHC
-- will tell you exactly what the problem is, so be sure to try /before/
-- uncommenting!



{- ONE -}

class Question1 (a :: Type -> Type)
-- instance Question1 Maybe

instance Question1 Maybe where



{- TWO -}

class Question2 (b :: Type -> Type -> Type)
-- instance Question2 Either
instance Question2 Either where
