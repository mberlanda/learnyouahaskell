{-
  import Control.Monad.Instances
    Module ‘Control.Monad.Instances’ is deprecated:
    This module now contains no instances and will be removed in the future
-}

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

addStuff' :: Int -> Int
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a+b
