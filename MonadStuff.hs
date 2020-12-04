module MonadStuff where

import Data.Foldable (find)

data Some a b = Yes b | No a deriving (Show, Eq)

instance Functor (Some a) where
    fmap _ (No x) = No x
    fmap f (Yes y) = Yes (f y)
